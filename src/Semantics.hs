module Semantics where

import Data.Bits
import Data.Char
import AST hiding (RawType(CHAN, CONST, VAR))
import qualified AST (RawType(CHAN, CONST, VAR))
import Reader hiding (location)
import Result

-- Type associated with a given name.
data Type = BYTE_ARRAY Integer
          | CHAN
          | CHAN_ARRAY Integer
          | CONST Type Value
          | VAR
          | VAR_ARRAY Integer
  deriving Eq

instance Show Type where
  show (BYTE_ARRAY n) = "VAR[BYTE " ++ show n ++ "]"
  show CHAN = "CHAN"
  show (CHAN_ARRAY n) = "CHAN[" ++ show n ++ "]"
  show (CONST t v) = "CONST " ++ show t ++ " " ++ show v
  show VAR = "VAR"
  show (VAR_ARRAY n) = "VAR[" ++ show n ++ "]"

-- Convert raw type to type.
raw_type :: AST.RawType -> Type
raw_type AST.CHAN = CHAN
raw_type AST.VAR = VAR
raw_type _ = error "This raw type should not appear in the AST."

-- Compile-time constant value.
data Value = Value Integer
           | Array [Integer]
           | ChanArray [Integer]
           | ByteArray String
  deriving Eq

instance Show Value where
  show (Value v) = show v
  show (Array vs) = show vs
  show (ByteArray cs) = show cs

-- Constant values.
two_pow_32 = 0x100000000
two_pow_31 = 0x10000000
true = 0xFFFFFFFF
false = 0x00000000

-- Compile-time computation on values.
value a = a `mod` two_pow_32
val_add a b = (a + b) `mod` two_pow_32
val_and a b = if a == true then b else false
val_bitwise_and a b = (a .&. b) `mod` two_pow_32
val_bitwise_or a b = (a .|. b) `mod` two_pow_32
val_bitwise_xor a b = (xor a b) `mod` two_pow_32
val_compare_eq a b = if a == b then true else false
val_compare_ge a b = if a >= b then true else false
val_compare_gt a b = if a > b then true else false
val_compare_le a b = if a <= b then true else false
val_compare_lt a b = if a < b then true else false
val_compare_ne a b = if a /= b then true else false
val_div a b = (a `div` b) `mod` two_pow_32
val_mod a b = (a `mod` b) `mod` two_pow_32
val_mul a b = (a * b) `mod` two_pow_32
val_neg a = (-a) `mod` two_pow_32
val_not a = (complement a) `mod` two_pow_32
val_or a b = if a == false then b else true
val_shift_left a b = (shift a (fromInteger b)) `mod` two_pow_32
val_shift_right a b = (shift a (-fromInteger b)) `mod` two_pow_32
val_sub a b = (a - b) `mod` two_pow_32

-- Information associated with a defined name.
type NameInfo = (Type, Location)

-- All names defined thus far.
type Environment = [(AST.Name, NameInfo)]

data State = State { environment :: Environment, has_error :: Bool }
  deriving Show

data SemanticAnalyser a = S (State -> IO (a, State))

instance Functor SemanticAnalyser where
  fmap f xm = xm >>= return . f

instance Applicative SemanticAnalyser where
  pure = return
  sf <*> sx = sf >>= (\f -> fmap f sx)

instance Monad SemanticAnalyser where
  return x = S (\state -> return (x, state))
  (S xm) >>= f = S (\state ->
    do
      (x, state') <- xm state
      case f x of
        S xm' -> xm' state')

empty_state = State { environment = [], has_error = False }

-- Print messages.
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

-- Save the environment, perform an analysis, then restore the environment.
new_scope :: SemanticAnalyser a -> SemanticAnalyser a
new_scope analyser =
  get_env >>= (\env ->
    analyser >>= (\a ->
      set_env env >> return a))

-- Set the current environment.
add_name :: AST.Name -> NameInfo -> SemanticAnalyser ()
add_name name (t, loc) = do
  result <- find name
  case result of
    Nothing -> return ()
    Just (_, loc') ->
      print_warning loc (
          "Declaration of '" ++ name ++ "' shadows existing declaration at " ++
          show loc' ++ ".")
  get_env >>= (\env -> set_env ((name, (t, loc)) : env))

-- Look up a name in the environment.
find :: AST.Name -> SemanticAnalyser (Maybe NameInfo)
find name = get_env >>= (\env -> return $ lookup name env)

run_analyser :: SemanticAnalyser a -> IO (a, State)
run_analyser (S xm) = xm empty_state

check_process :: L Process -> SemanticAnalyser ()
check_process (L p loc) =
  case p of
    Alt a -> return ()
    Assign l r -> return ()
    Call n expr -> return ()
    Definition ds p -> do
      check_definition ds
      check_process p
    Delay expr -> return ()
    If cond -> return ()
    Input l r -> return ()
    Output l r -> return ()
    Par rep -> return ()
    PriorityAlt a -> return ()
    PriorityPar p -> return ()
    Seq rep -> return ()
    Skip -> return ()
    Stop -> return ()
    Timer expr -> return ()
    While expr proc -> return ()

-- Check that a definition is legal, and update the environment.
check_definition :: [L Definition] -> SemanticAnalyser ()
check_definition [] = return ()
check_definition ((L d loc):ds) =
  (case d of
     DefineSingle t name -> do
       add_name name (raw_type t, loc)
     DefineVector t name l_expr -> do
       (t', value) <- check_and_compute_constexpr l_expr
       case value of
         Value size ->
           add_name name (VAR_ARRAY size, loc)
         _ -> type_mismatch (location l_expr) VAR t'
     DefineConstant name l_expr -> do
       (t, value) <- check_and_compute_constexpr l_expr
       add_name name (CONST t value, loc)
     _ -> return ()) >> check_definition ds

-- Check that a constant expression is actually constant, and return the
-- calculated compile-time value.
check_and_compute_constexpr :: L Expression -> SemanticAnalyser (Type, Value)
check_and_compute_constexpr (L expr loc) =
  case expr of
    Add es -> assoc val_add 0 es >>= (\x -> return (VAR, Value x))
    After a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_lt (val_sub x1 x2) two_pow_31)
    And es -> assoc val_and true es >>= (\x -> return (VAR, Value x))
    Any -> do
      print_error loc "Invalid use of ANY."
      return (VAR, Value 0)
    BitwiseAnd es ->
      assoc val_bitwise_and true es >>= (\x -> return (VAR, Value x))
    BitwiseOr es ->
      assoc val_bitwise_or false es >>= (\x -> return (VAR, Value x))
    BitwiseXor es ->
      assoc val_bitwise_xor false es >>= (\x -> return (VAR, Value x))
    CompareEQ a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_eq x1 x2)
    CompareGE a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_ge x1 x2)
    CompareGT a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_gt x1 x2)
    CompareLE a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_le x1 x2)
    CompareLT a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_lt x1 x2)
    CompareNE a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_compare_ne x1 x2)
    Div a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_div x1 x2)
    Index a (array_type, b) -> do
      -- Compute the table and the index.
      (t, v) <- check_and_compute_constexpr a
      i <- check_and_compute_value b
      -- Check that the table and index types match.
      case v of
        Array vs ->
          if array_type == INT then
            return (VAR, Value $ vs !! (fromInteger i))
          else do
            print_error loc "Byte-access to word-arrays is unimplemented."
            return (VAR, Value 0)
        ByteArray vs ->
          if array_type == BYTE then
            return (VAR, Value . toInteger . ord $ vs !! (fromInteger i))
          else do
            print_error loc "Word-access to byte-arrays is unimplemented."
            return (VAR, Value 0)
        _ -> type_mismatch
                 loc (if array_type == INT then VAR_ARRAY 0 else BYTE_ARRAY 0) t
    Literal l ->
      case l of
        Bool b -> return (VAR, Value $ if b then true else false)
        Char c -> return (VAR, Value . toInteger . ord $ c)
        Integer i -> return (VAR, Value $ value i)
        String s -> return (BYTE_ARRAY . toInteger $ (length s), ByteArray s)
        Table INT es -> do
          vs <- mapM check_and_compute_value es
          return (VAR_ARRAY . toInteger $ length vs, Array vs)
        Table BYTE es -> do
          vs <- mapM check_and_compute_value es
          return (BYTE_ARRAY . toInteger $ length vs,
                  ByteArray . map (chr . fromInteger . (`mod` 256)) $ vs)
    Mod a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_mod x1 x2)
    Mul es ->
      assoc val_mul 1 es >>= (\x -> return (VAR, Value x))
    Neg a -> do
      x <- check_and_compute_value a
      return (VAR, Value $ val_neg x)
    Not a -> do
      x <- check_and_compute_value a
      return (VAR, Value $ val_not x)
    Or es ->
      assoc val_or false es >>= (\x -> return (VAR, Value x))
    ShiftLeft a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_shift_left x1 x2)
    ShiftRight a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_shift_right x1 x2)
    Slice vec (array_type, a, b) -> do
      -- Generate the table and slice indices.
      (t, v) <- check_and_compute_constexpr vec
      i <- check_and_compute_value a
      j <- check_and_compute_value b
      if i < 0 then do  -- Check that the start is at least at 0.
        print_error loc ("Negative start index in slice: " ++ show i ++ ".")
        return (VAR_ARRAY 0, Array [])
      else if j < 0 then do  -- Check that the slice non-negative.
        print_error loc ("Negative range in slice: " ++ show j ++ ".")
        return (VAR_ARRAY 0, Array [])
      else
        -- Check that the table and slicer have the same type.
        case v of
          Array vs ->
            -- Check that the slice doesn't extend past the end of the array.
            if i + j > toInteger (length vs) then do
              print_error loc ("Slice extends past end of array: [" ++ show i ++
                               " FOR " ++ show j ++ "]")
              return (VAR_ARRAY 0, Array [])
            else if array_type == INT then
              return (VAR_ARRAY j,
                      Array (take (fromInteger j) .
                             drop (fromInteger i) $ vs))
            else do
              print_error loc "Byte-access to word-arrays is unimplemented."
              return (BYTE_ARRAY 0, ByteArray [])
          ByteArray vs ->
            -- Check that the slice doesn't extend past the end of the array.
            if i + j > toInteger (length vs) then do
              print_error loc ("Slice extends past end of array: [" ++ show i ++
                               " FOR " ++ show j ++ "]")
              return (VAR_ARRAY 0, Array [])
            else if array_type == BYTE then
              return (BYTE_ARRAY j,
                      ByteArray (take (fromInteger j) .
                                 drop (fromInteger i) $ vs))
            else do
              print_error loc "Word-access to byte-arrays is unimplemented."
              return (VAR, Value 0)
          _ -> if array_type == INT then
                 type_mismatch loc (VAR_ARRAY 0) t
               else
                 type_mismatch loc (BYTE_ARRAY 0) t
    Sub a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (VAR, Value $ val_sub x1 x2)
    Variable n -> do
      result <- find n
      case result of
        Nothing -> do
          print_error loc ("Undefined name '" ++ n ++ "'.")
          return (VAR, Value 0)
        Just (t, loc') ->
          case t of
            CONST def_type def_value -> return (def_type, def_value)
            _ -> do
              print_error loc
                          ("Use of non-constant name '" ++ n ++
                           "' (defined at " ++ show loc' ++ ") in constant " ++
                           "expression.")
              return (VAR, Value 0)
  where assoc f e [] = return e
        assoc f e (l_e:es) = do
          (t, v) <- check_and_compute_constexpr l_e
          case v of
            Value x -> assoc f e es >>= (\y -> return $ f x y)
            _ -> type_mismatch (location l_e) VAR t
        check_and_compute_value a = do
          (t, v) <- check_and_compute_constexpr a
          x <- case v of
                 Value x -> return x
                 _ -> type_mismatch (location a) VAR t
          return x
