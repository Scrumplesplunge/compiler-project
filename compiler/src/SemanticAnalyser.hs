module SemanticAnalyser where

import AST (L (L))
import qualified AST
import Data.Int
import Data.List
import Control.Monad
import AnnotatedAST
import Reader
import Static
import System.IO

-- Information associated with a defined name.
type NameInfo = (Type, Location)

-- All names defined thus far.
type Environment = [(AST.Name, NameInfo)]

data State = State {
  num_errors :: Integer,
  num_warnings :: Integer,
  next_static_address :: Int32,  -- Next address to assign to static data.
  static :: [(Int32, Static)]    -- Static data currently defined.
}

type StaticChain = [Int32]

instance Show State where
  show s = show_data
    where list f x = concat . intersperse "\n" . reverse . map f $ x
          show_data = list show_blob (static s)
          show_blob (location, value) =
            show (Address location) ++ ":\t " ++ show value

data SemanticAnalyser a = S (Environment -> StaticChain -> State -> IO (a, State))

instance Functor SemanticAnalyser where
  fmap f xm = xm >>= return . f

instance Applicative SemanticAnalyser where
  pure = return
  sf <*> sx = sf >>= (\f -> fmap f sx)

instance Monad SemanticAnalyser where
  return x = S (\env chain state -> return (x, state))
  (S xm) >>= f = S (\env chain state -> do
    (x, state') <- xm env chain state
    case f x of
      S xm' -> xm' env chain state')

empty_state = State {
  num_warnings = 0,
  num_errors = 0,
  next_static_address = 0,
  static = []
}

-- Print messages.
putStdErr :: String -> SemanticAnalyser ()
putStdErr x = S (\env chain state -> do
  hPutStrLn stderr x
  return ((), state))

print_note :: String -> SemanticAnalyser ()
print_note message =
  putStdErr ("Note: " ++ message)

print_warning :: Location -> String -> SemanticAnalyser ()
print_warning loc message = do
  putStdErr ("Warning at " ++ show loc ++ ": " ++ message)
  S (\env chain state ->
       return ((), state { num_warnings = num_warnings state + 1 }))

print_error :: Location -> String -> SemanticAnalyser ()
print_error loc message = do
  putStdErr ("Error at " ++ show loc ++ ": " ++ message)
  S (\env chain state ->
       return ((), state { num_errors = num_errors state + 1 }))

print_fatal :: Location -> String -> SemanticAnalyser a
print_fatal loc message =
  error ("FATAL Error at " ++ show loc ++ ": " ++ message)

type_mismatch :: Location -> Type -> Type -> SemanticAnalyser a
type_mismatch loc expected actual =
  print_fatal loc
              ("Expected " ++ show expected ++ ", got " ++ show actual ++ ".")

-- Get the current state of the environment.
get_env :: SemanticAnalyser Environment
get_env = S (\env chain state -> return (env, state))

-- Get the current level in the static chain.
get_level :: SemanticAnalyser Integer
get_level = S (\env chain state -> return (fromIntegral $ length chain, state))

-- Allocate space in the static chain. Return the allocation.
alloc :: Int32 -> (Allocation -> SemanticAnalyser a) -> SemanticAnalyser a
alloc x f = do
  -- Fetch the static chain.
  chain <- S (\env chain state -> return (chain, state))

  -- Compute the allocation.
  let static_level = (fromIntegral . length) chain - 1
  let old_value = head chain
  let new_value = old_value - x
  let chain' = new_value : tail chain

  -- Allocate the space.
  S (\env chain state ->
      let (S analyser) = f (Local static_level new_value)
      in analyser env chain' state)

-- Enter a new static level.
new_level :: SemanticAnalyser a -> SemanticAnalyser a
new_level (S analyser) =
  S (\env chain state -> analyser env (0 : chain) state)

-- Register a new static blob.
add_static :: Static -> SemanticAnalyser Int32
add_static s = do
  -- Read the next static address to assign.
  address <- S (\env chain state -> return (next_static_address state, state))
  -- Read the static index (ie. the current number of static objects).
  id <- S (\env chain state -> return (length (static state), state))
  -- Compute the static data blob size.
  let size = case s of
               WordArray ws -> 4 * fromIntegral (length ws)
               ByteArray bs -> 4 * ((fromIntegral (length bs) + 3) `div` 4)
  -- Store the blob.
  S (\env chain state ->
    let state' = state {
          next_static_address = address + size,
          static = (address, s) : (static state) }
    in return ((), state'))
  -- Return the address.
  return address

-- Access a static blob using its address. Note that access to the innards of
-- the blob is not supported.
lookup_address :: Int32 -> [(Int32, Static)] -> Maybe (Int32, Static)
lookup_address address [] = Nothing
lookup_address address ((a, v) : as) =
  if a <= address then Just (a, v) else lookup_address address as

get_static :: Location -> Int32 -> SemanticAnalyser Static
get_static loc address = do
  ss <- S (\env chain state -> return (static state, state))
  case lookup_address address ss of
    Nothing -> print_fatal loc "Invalid address used to access static data."
    Just (a, WordArray ws) -> do
      let index = (address - a) `div` 4
      if (address `mod` 4) /= 0 then do
        print_warning loc "Unaligned access to word array. Treating as aligned."
      else if index >= fromIntegral (length ws) then
        print_error loc "Static array access out of bounds."
      else
        return ()
      return (WordArray (drop (fromIntegral index) ws))
    Just (a, ByteArray bs) -> do
      let index = address - a
      if address >= fromIntegral (length bs) then
        print_error loc "Static array access out of bounds."
      else
        return ()
      return (ByteArray (drop (fromIntegral index) bs))

-- Set the current environment.
add_name :: AST.Name -> (Type, Location) -> SemanticAnalyser a
         -> SemanticAnalyser a
add_name name (t, loc) (S analyser) = do
  env <- get_env
  case lookup name env of
    Nothing -> return ()
    Just (_, loc') ->
      print_warning loc (
          "Declaration of '" ++ name ++ "' shadows existing declaration at " ++
          show loc' ++ ".")
  S (\env chain state -> analyser ((name, (t, loc)) : env) chain state)

-- Find a anme in the environment.
find_name name = do
  env <- get_env
  case lookup name env of
    Nothing ->
      return Nothing
    Just x -> do
      return (Just x)

run_analyser :: SemanticAnalyser a -> State -> IO (a, State)
run_analyser (S xm) state = xm [] [0] state
