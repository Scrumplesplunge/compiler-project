module SemanticAnalyser where

import AST (L (L))
import qualified AST
import Data.List
import Control.Monad
import AnnotatedAST
import Reader

-- Information associated with a defined name.
type NameInfo = (Type, Location)

-- All names defined thus far.
type Environment = [(AST.Name, NameInfo)]

-- Compile-time constant arrays.
data Static = WordArray [Integer]  -- Constant array of words.
            | ByteArray String     -- Constant array of bytes.
  deriving (Eq, Show)

data State = State {
  environment :: Environment,      -- Variables currently in scope.
  has_error :: Bool,
  next_static_address :: Integer,  -- Next address to be assigned to static data.
  static :: [(Integer, Static)]    -- Static data currently defined.
}

instance Show State where
  show s = show_env ++ "\n\n" ++ show_data
    where list f x = concat . intersperse "\n" . reverse . map f $ x
          show_env = list show_var (environment s)
          show_data = list show_blob (static s)
          show_var (n, (t, loc)) =
            show_compact loc ++ ":\t " ++ show n ++ " :: " ++ show t
          show_blob (location, value) =
            show (Address location) ++ ":\t " ++ show value

data SemanticAnalyser a = S (State -> IO (a, State))

instance Functor SemanticAnalyser where
  fmap f xm = xm >>= return . f

instance Applicative SemanticAnalyser where
  pure = return
  sf <*> sx = sf >>= (\f -> fmap f sx)

instance Monad SemanticAnalyser where
  return x = S (\state -> return (x, state))
  (S xm) >>= f = S (\state -> do
    (x, state') <- xm state
    case f x of
      S xm' -> xm' state')

empty_state = State {
  environment = [],
  has_error = False,
  next_static_address = mem_start,
  static = []
}

-- Print messages.
print_note :: String -> SemanticAnalyser ()
print_note message =
  S (\state -> do
    putStrLn ("Note: " ++ message)
    return ((), state))

print_warning :: Location -> String -> SemanticAnalyser ()
print_warning loc message =
  S (\state -> do
    putStrLn ("Warning at " ++ show loc ++ ": " ++ message)
    return ((), state))

print_error :: Location -> String -> SemanticAnalyser ()
print_error loc message =
  S (\state -> do
    putStrLn ("Error at " ++ show loc ++ ": " ++ message)
    return ((), state { has_error = True }))

print_fatal :: Location -> String -> SemanticAnalyser a
print_fatal loc message =
  S (\state -> error ("FATAL Error at " ++ show loc ++ ": " ++ message))

type_mismatch :: Location -> Type -> Type -> SemanticAnalyser a
type_mismatch loc expected actual =
  print_fatal loc
              ("Expected " ++ show expected ++ ", got " ++ show actual ++ ".")

-- Get the current state of the environment.
get_env :: SemanticAnalyser Environment
get_env = S (\state -> return (environment state, state))

-- Set the environment.
set_env :: Environment -> SemanticAnalyser ()
set_env env = S (\state -> return ((), state { environment = env }))

-- Register a new static blob.
add_static :: Static -> SemanticAnalyser Integer
add_static s = do
  -- Read the next static address to assign.
  address <- S (\state -> return (next_static_address state, state))
  -- Read the static index (ie. the current number of static objects).
  id <- S (\state -> return (length (static state), state))
  -- Compute the static data blob size.
  let size = case s of
               WordArray ws -> 4 * toInteger (length ws)
               ByteArray bs -> 4 * ((toInteger (length bs) + 3) `div` 4)
  -- Store the blob.
  S (\state ->
    let state' = state {
          next_static_address = address + size,
          static = (address, s) : (static state) }
    in return ((), state'))
  -- Return the address.
  return address

-- Access a static blob using its address. Note that access to the innards of
-- the blob is not supported.
lookup_address :: Integer -> [(Integer, Static)] -> Maybe (Integer, Static)
lookup_address address [] = Nothing
lookup_address address ((a, v) : as) =
  if a <= address then Just (a, v) else lookup_address address as

get_static :: Location -> Integer -> SemanticAnalyser Static
get_static loc address = do
  ss <- S (\state -> return (static state, state))
  case lookup_address address ss of
    Nothing -> print_fatal loc "Invalid address used to access static data."
    Just (a, WordArray ws) -> do
      let index = (address - a) `div` 4
      if (address `mod` 4) /= 0 then do
        print_warning loc "Misaligned access to word array. Treating as aligned."
      else if index >= toInteger (length ws) then
        print_error loc "Static array access out of bounds."
      else
        return ()
      return (WordArray (drop (fromInteger index) ws))
    Just (a, ByteArray bs) -> do
      let index = address - a
      if address >= toInteger (length bs) then
        print_error loc "Static array access out of bounds."
      else
        return ()
      return (ByteArray (drop (fromInteger index) bs))

-- Save the environment, perform an analysis, then restore the environment.
new_scope :: SemanticAnalyser a -> SemanticAnalyser a
new_scope analyser = do
  env <- get_env
  a <- analyser
  set_env env
  return a

-- Set the current environment.
add_name :: AST.Name -> NameInfo -> SemanticAnalyser ()
add_name name (t, loc) = do
  result <- find_name name
  case result of
    Nothing -> return ()
    Just (_, loc') ->
      print_warning loc (
          "Declaration of '" ++ name ++ "' shadows existing declaration at " ++
          show loc' ++ ".")
  get_env >>= (\env -> set_env ((name, (t, loc)) : env))

-- Look up a name in the environment.
find_name :: AST.Name -> SemanticAnalyser (Maybe NameInfo)
find_name name = get_env >>= (\env -> return $ lookup name env)

run_analyser :: SemanticAnalyser a -> IO (a, State)
run_analyser (S xm) = xm empty_state
