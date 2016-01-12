module Semantics where

import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.List
import AST (L (L))
import qualified AST
import AnnotatedAST
import Reader hiding (location)
import Result

-- Information associated with a defined name.
type NameInfo = (Type, Location)

-- All names defined thus far.
type Environment = [(AST.Name, NameInfo)]

data State = State { environment :: Environment, has_error :: Bool }

instance Show State where
  show s = concat . Data.List.intersperse "\n" . reverse . map display $ environment s
    where display (n, (t, loc)) =
            show_compact loc ++ ":\t" ++ show n ++ " :: " ++ show t

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

-- Helpers for the replicable and nestable types.
check_replicator :: AST.Replicator -> SemanticAnalyser Replicator
check_replicator (AST.Range (L n loc') a b) = do
  add_name n (INT, loc')
  a' <- check_rvalue a
  b' <- check_rvalue b
  return (Range n a' b')

check_replicable :: (a -> SemanticAnalyser a2) -> AST.Replicable a
                 -> SemanticAnalyser (Replicable a2)
check_replicable check_a (AST.Basic as) = do
  as' <- mapM check_a as
  return (Basic as')
check_replicable check_a (AST.Replicated r a) =
  new_scope (do
    r' <- check_replicator r
    a' <- check_a a
    return (Replicated r' a'))

check_nestable :: (a -> SemanticAnalyser a2) -> (b -> SemanticAnalyser b2)
               -> AST.Nestable a b -> SemanticAnalyser (Nestable a2 b2)
check_nestable check_a check_b (AST.Nested a) = do
  a' <- check_a a
  return (Nested a')
check_nestable check_a check_b (AST.Block b p) = do
  b' <- check_b b
  p' <- check_process p
  return (Block b' p')

-- Check that a name is defined.
check_name :: (Type -> Bool) -> Location -> String -> SemanticAnalyser ()
check_name check_type loc x = do
  x' <- find x
  case x' of
    Nothing ->
      print_error loc ("Undefined name " ++ show x)
    Just (t, loc') ->
      case t of
        CONST t' _ ->
          if check_type t' then
            return ()
          else do
            print_error loc
                ("Unexpected name " ++ show x ++ " of type " ++ show t ++ ".")
            print_note (show x ++ " is defined at " ++ show loc')
        _ ->
          if check_type t then
            return ()
          else do
            print_error loc
                ("Unexpected name " ++ show x ++ " of type " ++ show t ++ ".")
            print_note (show x ++ " is defined at " ++ show loc')

check_process :: L AST.Process -> SemanticAnalyser Process
check_process (L p loc) =
  case p of
    AST.Alt a -> check_alt (L a loc) >>= return . Alt
    AST.Assign l r -> check_assign l r
    AST.Call n es -> check_call n es
    AST.Definition ds p -> check_definition ds p
    AST.Delay expr -> check_delay expr
    AST.If cond -> check_if (L cond loc) >>= return . If
    AST.Input l r -> check_input l r
    AST.Output l r -> check_output l r
    AST.Par rep -> check_par rep >>= return . Par
    AST.PriorityAlt a -> check_alt (L a loc) >>= return . PriorityAlt
    AST.PriorityPar p -> check_pripar p >>= return . PriorityPar
    AST.Seq rep -> check_seq rep >>= return . Seq
    AST.Skip -> return Skip
    AST.Stop -> return Stop
    AST.Timer expr -> check_timer expr >>= return . Timer
    AST.While expr proc -> check_while expr proc

check_alt :: L AST.Alternative -> SemanticAnalyser Alternative
check_alt (L (AST.Alternative a) loc) = do
  a' <- check_replicable (check_nestable check_alt check_guard) a
  return (Alternative a')

check_guard :: L AST.Guard -> SemanticAnalyser Guard
check_guard (L (AST.BasicGuard a) loc) = do
  a' <- check_atomic_guard a
  return (BasicGuard a')

check_guard (L (AST.PrefixedGuard e a) loc) = do
  e' <- check_rvalue e
  a' <- check_atomic_guard a
  return (PrefixedGuard e' a')

check_atomic_guard :: L AST.AtomicGuard -> SemanticAnalyser AtomicGuard
check_atomic_guard (L (AST.DelayGuard e) loc) = do
  e' <- check_rvalue e
  return (DelayGuard e')

check_atomic_guard (L (AST.InputGuard a bs) loc) = do
  a' <- check_channel a
  bs' <- mapM check_lvalue bs
  return (InputGuard a' bs')

check_atomic_guard (L AST.SkipGuard loc) = return SkipGuard

check_assign :: L AST.Expression -> L AST.Expression -> SemanticAnalyser Process
check_assign l r = do
  -- TODO: Type-check the variables.
  l' <- check_lvalue l
  r' <- check_rvalue r
  return (Assign l' r')

check_call :: L Name -> [L AST.Expression] -> SemanticAnalyser Process
check_call (L n loc) es = do
  -- TODO: Type-check the arguments.
  es' <- mapM check_rvalue es
  return (Call n es')

check_definition :: [L AST.Definition] -> L AST.Process
                 -> SemanticAnalyser Process
check_definition [] p = check_process p
check_definition ((L d loc) : ds) p = do
  case d of
    AST.DefineSingle t name -> do
      -- Define the variable.
      add_name name (raw_type t, loc)
    AST.DefineVector t name l_expr -> do
      -- Compute the (constant) size of the vector.
      (t', value) <- check_and_compute_constexpr l_expr
      case value of
        Integer size ->
          -- Define the array.
          add_name name (INT_ARRAY (CompileTime size), loc)
        _ -> type_mismatch (AST.location l_expr) INT t'
    AST.DefineConstant name l_expr -> do
      -- Compute the constant value.
      (t, value) <- check_and_compute_constexpr l_expr
      add_name name (CONST t value, loc)
    AST.DefineProcedure name formals proc -> do
      t <- new_scope (do
        formals' <- mapM check_formal formals
        proc' <- check_process proc
        return (PROC formals' proc'))
      add_name name (t, loc)
  check_definition ds p

check_formal :: L AST.Formal -> SemanticAnalyser Type
check_formal (L (AST.Single r n) loc) = do
  let t = raw_type r
  add_name n (t, loc)
  return t

check_formal (L (AST.Vector r n) loc) = do
  let t = (case raw_type r of
             BYTE -> BYTE_ARRAY Runtime
             CHAN -> CHAN_ARRAY Runtime
             INT -> INT_ARRAY Runtime)
  add_name n (t, loc)
  return t

check_delay :: L AST.Expression -> SemanticAnalyser Process
check_delay expr = do
  expr' <- check_rvalue expr
  return (Delay expr')

check_if :: L AST.Condition -> SemanticAnalyser Condition
check_if (L (AST.Condition cond) loc) = do
  cond' <- check_replicable (check_nestable check_if check_rvalue) cond
  return (Condition cond')

check_input :: L AST.Expression -> L AST.Expression -> SemanticAnalyser Process
check_input l r = do
  l' <- check_channel l
  r' <- check_lvalue r
  return (Input l' r')

check_output :: L AST.Expression -> L AST.Expression
             -> SemanticAnalyser Process
check_output l r = do
  l' <- check_channel l
  r' <- check_rvalue r
  return (Output l' r')

check_par :: AST.Replicable (L AST.Process)
          -> SemanticAnalyser (Replicable Process)
check_par par = check_replicable check_process par

check_pripar :: AST.Replicable (L AST.Process)
             -> SemanticAnalyser (Replicable Process)
check_pripar par =
  -- TODO: Verify that the limit on the number of priority par's is not broken.
  check_par par

check_seq :: AST.Replicable (L AST.Process)
          -> SemanticAnalyser (Replicable Process)
check_seq seq = check_replicable check_process seq

check_timer :: L AST.Expression -> SemanticAnalyser Expression
check_timer expr = check_rvalue expr

check_while :: L AST.Expression -> L AST.Process -> SemanticAnalyser Process
check_while expr proc = do
  expr' <- check_rvalue expr
  proc' <- check_process proc
  return (While expr' proc')

-- Check an expression.
check_rvalue :: L AST.Expression -> SemanticAnalyser Expression
check_rvalue (L expr loc) = do
  case expr of
    AST.Add es -> do
      es' <- mapM check_rvalue es
      return (Add es')
    AST.After a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (After a' b')
    AST.And es -> do
      es' <- mapM check_rvalue es
      return (And es')
    AST.Any -> return Any
    AST.BitwiseAnd es -> do
      es' <- mapM check_rvalue es
      return (BitwiseAnd es')
    AST.BitwiseOr es -> do
      es' <- mapM check_rvalue es
      return (BitwiseOr es')
    AST.BitwiseXor es -> do
      es' <- mapM check_rvalue es
      return (BitwiseXor es')
    AST.CompareEQ a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (CompareEQ a' b')
    AST.CompareGE a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (CompareGE a' b')
    AST.CompareGT a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (CompareGT a' b')
    AST.CompareLE a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (CompareLE a' b')
    AST.CompareLT a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (CompareLT a' b')
    AST.CompareNE a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (CompareNE a' b')
    AST.Div a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Div a' b')
    AST.Index r (t, i) -> do
      -- TODO: Check that the array type matches the index type (or at least
      -- that cross-type support is present).
      r' <- check_array r
      i' <- check_rvalue i
      return (Index r' (t, i'))
    AST.Literal l -> do
      (t, v) <- check_and_compute_constexpr (L (AST.Literal l) loc)
      return (Value v)
    AST.Mod a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Mod a' b')
    AST.Mul es -> do
      es' <- mapM check_rvalue es
      return (Mul es')
    AST.Neg a -> do
      a' <- check_rvalue a
      return (Neg a')
    AST.Not a -> do
      a' <- check_rvalue a
      return (Not a')
    AST.Or es -> do
      es' <- mapM check_rvalue es
      return (Or es')
    AST.ShiftLeft a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (ShiftLeft a' b')
    AST.ShiftRight a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (ShiftRight a' b')
    AST.Slice r (t, a, b) -> do
      -- TODO: Check that the array type matches the index type (or at least
      -- that cross-type support is present).
      r' <- check_array r
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Slice r' (t, a', b'))
    AST.Sub a b -> do
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Sub a' b')
    AST.Variable x -> do
      check_name (const True) loc x
      return (Name x)

-- Check that a constant expression is actually constant, and return the
-- calculated compile-time value.
check_and_compute_constexpr :: L AST.Expression
                            -> SemanticAnalyser (Type, Value)
check_and_compute_constexpr (L expr loc) =
  case expr of
    AST.Add es -> do
      x <- assoc val_add 0 es
      return (INT, Integer x)
    AST.After a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_lt (val_sub x1 x2) two_pow_31)
    AST.And es -> do
      x <- assoc val_and true es
      return (INT, Integer x)
    AST.Any -> do
      print_error loc "Invalid use of ANY."
      return (INT, Integer 0)
    AST.BitwiseAnd es -> do
      x <- assoc val_bitwise_and true es
      return (INT, Integer x)
    AST.BitwiseOr es -> do
      x <- assoc val_bitwise_or false es
      return (INT, Integer x)
    AST.BitwiseXor es -> do
      x <- assoc val_bitwise_xor false es
      return (INT, Integer x)
    AST.CompareEQ a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_eq x1 x2)
    AST.CompareGE a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_ge x1 x2)
    AST.CompareGT a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_gt x1 x2)
    AST.CompareLE a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_le x1 x2)
    AST.CompareLT a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_lt x1 x2)
    AST.CompareNE a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_compare_ne x1 x2)
    AST.Div a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_div x1 x2)
    AST.Index a (array_type, b) -> do
      -- Compute the table and the index.
      (t, v) <- check_and_compute_constexpr a
      i <- check_and_compute_value b
      -- Check that the table and index types match.
      case v of
        Array vs ->
          if array_type == AST.INT then
            return (INT, Integer $ vs !! (fromInteger i))
          else do
            print_error loc "Byte-access to word-arrays is unimplemented."
            return (INT, Integer 0)
        ByteArray vs ->
          if array_type == AST.BYTE then
            return (INT, Integer . toInteger . ord $ vs !! (fromInteger i))
          else do
            print_error loc "Word-access to byte-arrays is unimplemented."
            return (INT, Integer 0)
        _ -> type_mismatch
                 loc (if array_type == AST.INT then
                        INT_ARRAY Runtime
                      else
                        BYTE_ARRAY Runtime) t
    AST.Literal l ->
      case l of
        AST.Bool b -> return (INT, Integer $ if b then true else false)
        AST.Char c -> return (INT, Integer . toInteger . ord $ c)
        AST.Integer i -> return (INT, Integer $ value i)
        AST.String s ->
          return (BYTE_ARRAY . CompileTime . toInteger $ (length s),
                  ByteArray s)
        AST.Table AST.INT es -> do
          vs <- mapM check_and_compute_value es
          return (INT_ARRAY . CompileTime . toInteger $ length vs, Array vs)
        AST.Table AST.BYTE es -> do
          vs <- mapM check_and_compute_value es
          return (BYTE_ARRAY . CompileTime . toInteger $ length vs,
                  ByteArray . map (chr . fromInteger . (`mod` 256)) $ vs)
    AST.Mod a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_mod x1 x2)
    AST.Mul es -> do
      x <- assoc val_mul 1 es
      return (INT, Integer x)
    AST.Neg a -> do
      x <- check_and_compute_value a
      return (INT, Integer $ val_neg x)
    AST.Not a -> do
      x <- check_and_compute_value a
      return (INT, Integer $ val_not x)
    AST.Or es -> do
      x <- assoc val_or false es
      return (INT, Integer x)
    AST.ShiftLeft a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_shift_left x1 x2)
    AST.ShiftRight a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_shift_right x1 x2)
    AST.Slice vec (array_type, a, b) -> do
      -- Generate the table and slice indices.
      (t, v) <- check_and_compute_constexpr vec
      i <- check_and_compute_value a
      j <- check_and_compute_value b
      if i < 0 then do  -- Check that the start is at least at 0.
        print_error loc ("Negative start index in slice: " ++ show i ++ ".")
        return (INT_ARRAY Runtime, Array [])
      else if j < 0 then do  -- Check that the slice non-negative.
        print_error loc ("Negative range in slice: " ++ show j ++ ".")
        return (INT_ARRAY Runtime, Array [])
      else
        -- Check that the table and slicer have the same type.
        case v of
          Array vs ->
            -- Check that the slice doesn't extend past the end of the array.
            if i + j > toInteger (length vs) then do
              print_error loc ("Slice extends past end of array: [" ++ show i ++
                               " FOR " ++ show j ++ "]")
              return (INT_ARRAY Runtime, Array [])
            else if array_type == AST.INT then
              return (INT_ARRAY (CompileTime j),
                      Array (take (fromInteger j) .
                             drop (fromInteger i) $ vs))
            else do
              print_error loc "Byte-access to word-arrays is unimplemented."
              return (BYTE_ARRAY Runtime, ByteArray [])
          ByteArray vs ->
            -- Check that the slice doesn't extend past the end of the array.
            if i + j > toInteger (length vs) then do
              print_error loc ("Slice extends past end of array: [" ++ show i ++
                               " FOR " ++ show j ++ "]")
              return (INT_ARRAY Runtime, Array [])
            else if array_type == AST.BYTE then
              return (BYTE_ARRAY (CompileTime j),
                      ByteArray (take (fromInteger j) .
                                 drop (fromInteger i) $ vs))
            else do
              print_error loc "Word-access to byte-arrays is unimplemented."
              return (INT, Integer 0)
          _ -> if array_type == AST.INT then
                 type_mismatch loc (INT_ARRAY Runtime) t
               else
                 type_mismatch loc (BYTE_ARRAY Runtime) t
    AST.Sub a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_sub x1 x2)
    AST.Variable n -> do
      result <- find n
      case result of
        Nothing -> do
          print_error loc ("Undefined name '" ++ n ++ "'.")
          return (INT, Integer 0)
        Just (t, loc') ->
          case t of
            CONST def_type def_value -> return (def_type, def_value)
            _ -> do
              print_error loc
                          ("Use of non-constant name '" ++ n ++
                           "' (defined at " ++ show loc' ++ ") in constant " ++
                           "expression.")
              return (INT, Integer 0)
  where assoc f e [] = return e
        assoc f e (l_e:es) = do
          (t, v) <- check_and_compute_constexpr l_e
          case v of
            Integer x -> assoc f e es >>= (\y -> return $ f x y)
            _ -> type_mismatch (AST.location l_e) INT t

-- Compute the value of an integer const-expression, or produce an error if the
-- expression is not constant.
check_and_compute_value :: L AST.Expression -> SemanticAnalyser Integer
check_and_compute_value a = do
  (t, v) <- check_and_compute_constexpr a
  x <- case v of
         Integer x -> return x
         _ -> type_mismatch (AST.location a) INT t
  return x

-- Check that an expression evaluates to a channel.
check_channel :: L AST.Expression -> SemanticAnalyser Expression
check_channel (L chan loc) = do
  -- A channel is either named, or a member of an array.
  case chan of
    AST.Variable x -> do
      check_name (== CHAN) loc x
      return (Name x)
    AST.Index r (t, i) -> do
      r' <- check_channel_array r
      -- Array access ought to resemble INT indexing.
      if t == AST.INT then
        return ()
      else
        print_error loc "Invalid access to channel array."
      i' <- check_rvalue i
      return (Index r' (t, i'))
    _ -> print_fatal loc "Channel expected."

check_channel_array :: L AST.Expression -> SemanticAnalyser Expression
check_channel_array (L chan_array loc) = do
  -- A channel array is either a named array or a slice.
  case chan_array of
    AST.Variable x -> do
      check_name (\t ->
        case t of
          CHAN_ARRAY _ -> True
          _ -> False) loc x
      return (Name x)
    AST.Slice r (t, a, b) -> do
      r' <- check_channel_array r
      -- Array access ought to resemble INT slicing.
      if t == AST.INT then
        return ()
      else
        print_error loc "Invalid access to channel array."
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Slice r' (t, a', b'))
    _ -> print_fatal loc "Channel array expected."

check_lvalue :: L AST.Expression -> SemanticAnalyser Expression
check_lvalue (L expr loc) = do
  -- An lvalue is either a variable, an array index, or an array slice.
  -- It can alternatively be ANY, but this is a special case.
  case expr of
    AST.Any -> return Any
    AST.Variable x -> do
      check_name (\t ->
        case t of
          BYTE -> True
          BYTE_ARRAY _ -> True
          INT -> True
          INT_ARRAY _ -> True
          _ -> False) loc x
      return (Name x)
    AST.Index r (t, i) -> do
      -- TODO: Check that access type matches, or at least that cross-type
      -- access support is present.
      r' <- check_array r
      i' <- check_rvalue i
      return (Index r' (t, i'))
    AST.Slice r (t, a, b) -> do
      r' <- check_array r
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Slice r' (t, a', b'))
    _ -> print_fatal loc "L-value expected."

check_array :: L AST.Expression -> SemanticAnalyser Expression
check_array (L r loc) = do
  -- An array is either a named array or a slice.
  case r of
    AST.Variable x -> do
      check_name (\t ->
        case t of
          BYTE_ARRAY _ -> True
          INT_ARRAY _ -> True
          _ -> False) loc x
      return (Name x)
    AST.Slice r (t, a, b) -> do
      r' <- check_channel_array r
      -- Array access ought to resemble INT slicing.
      if t == AST.INT then
        return ()
      else
        print_error loc "Invalid access to channel array."
      a' <- check_rvalue a
      b' <- check_rvalue b
      return (Slice r' (t, a', b'))
    _ -> print_fatal loc "Array expected."
