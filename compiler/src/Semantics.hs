module Semantics where

-- import Control.Monad
-- import Data.Bits
import Data.Char
import Data.Int
import qualified Data.List
import AST (L (L))
import qualified AST
import AnnotatedAST
import SemanticAnalyser
import Static
import Reader hiding (location)
import Result

-- Helpers for the replicable and nestable types.
check_replicator :: Bool -> AST.Replicator -> SemanticAnalyser Replicator
check_replicator compile_time (AST.Range (L n _) a b) = do
  -- TODO: Check types.
  (t1, a') <- check_rvalue a
  if compile_time then do
    (t2, v) <- check_and_compute_constexpr b
    return (Range n a' (Value v))
  else do
    (t2, b') <- check_rvalue b
    return (Range n a' b')

check_replicable :: Bool -> (a -> SemanticAnalyser a2) -> AST.Replicable a
                 -> SemanticAnalyser (Replicable a2)
check_replicable compile_time check_a (AST.Basic as) = do
  as' <- mapM check_a as
  return (Basic as')
check_replicable
    compile_time check_a (AST.Replicated r@(AST.Range (L n loc) _ _) a) = do
  r' <- check_replicator compile_time r
  a' <- add_name n (INT, loc) (check_a a)
  return (Replicated r' a')

check_nestable :: (a -> SemanticAnalyser a2) -> (b -> SemanticAnalyser b2)
               -> AST.Nestable a b -> SemanticAnalyser (Nestable a2 b2)
check_nestable check_a check_b (AST.Nested a) = do
  a' <- check_a a
  return (Nested a')
check_nestable check_a check_b (AST.Block b p) = do
  b' <- check_b b
  p' <- check_process p
  return (Block b' p')

-- Check that a type is a numeric type.
check_numeric :: Location -> Type -> SemanticAnalyser ()
check_numeric loc t =
  case t of
    BYTE -> return ()
    CHAN -> return ()
    CONST t' _ -> check_numeric loc t'
    INT -> return ()
    ANY_TYPE -> return ()
    _ -> print_error loc ("Expected a numeric type, got " ++ show t ++ ".")

-- Check that a name is defined.
check_name :: (Type -> Bool) -> Location -> String -> SemanticAnalyser Name
check_name check_type loc x = do
  x' <- find_name x
  case x' of
    Nothing -> do
      print_error loc ("Undefined name " ++ show x)
      return ""
    Just (t, loc') -> do
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
      return x

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
    AST.DistPar p -> check_dist_par p >>= return . DistPar
    AST.Seq rep -> check_seq rep >>= return . Seq
    AST.Skip -> return Skip
    AST.Stop -> return Stop
    AST.Timer expr -> check_timer expr >>= return . Timer
    AST.While expr proc -> check_while expr proc

check_alt :: L AST.Alternative -> SemanticAnalyser Alternative
check_alt (L (AST.Alternative a) loc) = do
  a' <- check_replicable False (check_nestable check_alt check_guard) a
  return (Alternative a')

check_guard :: L AST.Guard -> SemanticAnalyser Guard
check_guard (L (AST.BasicGuard a) loc) = do
  a' <- check_atomic_guard a
  return (BasicGuard a')

check_guard (L (AST.PrefixedGuard e a) loc) = do
  -- TODO: Check type.
  (t, e') <- check_rvalue e
  check_numeric loc t
  a' <- check_atomic_guard a
  return (PrefixedGuard e' a')

check_atomic_guard :: L AST.AtomicGuard -> SemanticAnalyser AtomicGuard
check_atomic_guard (L (AST.DelayGuard e) loc) = do
  (t, e') <- check_rvalue e
  check_numeric loc t
  return (DelayGuard e')

check_atomic_guard (L (AST.InputGuard a bs) loc) = do
  a' <- check_channel a
  tbs' <- mapM check_lvalue bs
  mapM (check_numeric loc) (map fst tbs')
  return (InputGuard a' (map snd tbs'))

check_atomic_guard (L AST.SkipGuard loc) = return SkipGuard

check_assign :: L AST.Expression -> L AST.Expression -> SemanticAnalyser Process
check_assign l r = do
  (t1, l') <- check_lvalue l
  check_numeric (AST.location l) t1
  (t2, r') <- check_rvalue r
  check_numeric (AST.location r) t2
  return (Assign l' r')

check_call :: L AST.Name -> [L AST.Expression] -> SemanticAnalyser Process
check_call (L n loc) es = do
  -- TODO: Type-check the arguments.
  n' <- find_name n
  case n' of
    Nothing -> do
      print_error loc ("Name " ++ show n ++ " is undefined.")
      return (Call "" [])
    Just (t, loc') ->
      case t of
        PROC ts _ -> do
          tes' <- mapM check_rvalue es
          let es' = map snd tes'
          return (Call n es')
-- where f n [] [] = return []
--       f n [] (e : es) = do
--         print_error loc (show n ++ " is applied to too many arguments.")
--         return []
--       f n (t : ts) [] =
--         print_error loc (show n ++ " is applied to too few arguments.")
--         return []
--       f n (t : ts) (e : es) = do
--         case t of
--           BYTE -> do
--             (t', e') <- check_rvalue e
--             check_numeric (AST.location e) t
--             return e'
--           BYTE_ARRAY _ -> check_array e
--           CHAN -> check_channel e
--           CHAN_ARRAY _ -> check_channel_array e
--           INT -> do
--             (t', e') <- check_rvalue e
--             check_numeric (AST.location e) t
--             return e'
--           INT_ARRAY _ -> check_array e
        _ -> do
          print_error loc ("Name " ++ show n ++ " does not define a procedure.")
          print_note (show n ++ " is defined at " ++ show loc)
          return (Call "" [])

check_definition :: [L AST.Definition] -> L AST.Process
                 -> SemanticAnalyser Process
check_definition [] p = check_process p
check_definition ((L d loc) : ds) p = do
  case d of
    AST.DefineSingle t name -> do
      -- Define the variable.
      ds' <- add_name name (raw_type t, loc) $ check_definition ds p
      return $ Define t name 1 ds'
    AST.DefineVector t name l_expr -> do
      -- Compute the (constant) size of the vector.
      (t', value) <- check_and_compute_constexpr l_expr
      case value of
        Integer size -> do
          -- Define the array.
          ds' <- add_name name (raw_array_type t (fromIntegral size), loc)
                          (check_definition ds p)
          return $ Define t name (fromIntegral size) ds'
        _ -> do
          type_mismatch (AST.location l_expr) INT t'
          check_definition ds p
    AST.DefineConstant name l_expr -> do
      -- Compute the constant value.
      (t, value) <- check_and_compute_constexpr l_expr
      ds' <- add_name name (CONST t value, loc) $ check_definition ds p
      return $ DefineConstant name value ds'
    AST.DefineProcedure name formals proc -> do
      let formals' = formal_types formals
      proc' <- add_formals formals $ check_process proc
      let t = PROC formals' proc'
      ds' <- add_name name (t, loc) $ check_definition ds p
      return $ DefineProcedure name ds'

add_formals :: [L AST.Formal] -> SemanticAnalyser a -> SemanticAnalyser a
add_formals [] analyser = analyser
add_formals (f@(L (AST.Single r n) loc) : fs) analyser =
  add_name n (formal_type f, loc) analyser
add_formals (f@(L (AST.Vector r n) loc) : fs) analyser =
  add_name n (formal_type f, loc) analyser

formal_types :: [L AST.Formal] -> [Type]
formal_types = map formal_type

formal_type :: L AST.Formal -> Type
formal_type (L (AST.Single r n) loc) = raw_type r
formal_type (L (AST.Vector r n) loc) =
  case raw_type r of
    BYTE -> BYTE_ARRAY_REF
    CHAN -> CHAN_ARRAY_REF
    INT -> INT_ARRAY_REF

check_delay :: L AST.Expression -> SemanticAnalyser Process
check_delay expr = do
  -- TODO: Check type.
  (t, expr') <- check_rvalue expr
  return (Delay expr')

check_condition :: L AST.Expression -> SemanticAnalyser Expression
check_condition expr = do
  -- TODO: Check type.
  (t, expr') <- check_rvalue expr
  return expr'

check_if :: L AST.Condition -> SemanticAnalyser Condition
check_if (L (AST.Condition cond) loc) = do
  cond' <- check_replicable False (check_nestable check_if check_condition) cond
  return (Condition cond')

check_input :: L AST.Expression -> L AST.Expression -> SemanticAnalyser Process
check_input l r = do
  -- TODO: Check types.
  l' <- check_channel l
  (t, r') <- check_lvalue r
  return (Input l' r')

check_output :: L AST.Expression -> L AST.Expression
             -> SemanticAnalyser Process
check_output l r = do
  l' <- check_channel l
  -- TODO: Check type.
  (t, r') <- check_rvalue r
  return (Output l' r')

check_par :: AST.Replicable (L AST.Process)
          -> SemanticAnalyser (Replicable Process)
check_par par = check_replicable True check_process par

check_dist_par :: AST.Replicable (L AST.Process)
             -> SemanticAnalyser (Replicable Process)
check_dist_par par = check_replicable True check_process par

check_seq :: AST.Replicable (L AST.Process)
          -> SemanticAnalyser (Replicable Process)
check_seq seq = check_replicable False check_process seq

check_timer :: L AST.Expression -> SemanticAnalyser Expression
check_timer expr = do
  -- TODO: Check type.
  (t, expr') <- check_rvalue expr
  return expr'

check_while :: L AST.Expression -> L AST.Process -> SemanticAnalyser Process
check_while expr proc = do
  -- TODO: Check type.
  (t, expr') <- check_rvalue expr
  proc' <- check_process proc
  return (While expr' proc')

-- Check an expression.
check_rvalue :: L AST.Expression -> SemanticAnalyser (Type, Expression)
check_rvalue (L expr loc) = do
  case expr of
    AST.Add es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, Add (map snd tes'))
    AST.After a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, After a' b')
    AST.And es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, And (map snd tes'))
    AST.Any -> return (ANY_TYPE, Any)
    AST.BitwiseAnd es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, BitwiseAnd (map snd tes'))
    AST.BitwiseOr es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, BitwiseOr (map snd tes'))
    AST.BitwiseXor es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, BitwiseXor (map snd tes'))
    AST.CompareEQ a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, CompareEQ a' b')
    AST.CompareGE a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, CompareGE a' b')
    AST.CompareGT a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, CompareGT a' b')
    AST.CompareLE a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, CompareLE a' b')
    AST.CompareLT a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, CompareLT a' b')
    AST.CompareNE a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, CompareNE a' b')
    AST.Div a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, Div a' b')
    AST.Index r (t, i) -> do
      -- TODO: Check that the array type matches the index type (or at least
      -- that cross-type support is present).
      r' <- check_array r
      (t', i') <- check_rvalue i
      return (INT, Index r' (t, i'))
    AST.Literal l -> do
      (t, v) <- check_and_compute_constexpr (L (AST.Literal l) loc)
      return (t, Value v)
    AST.Mod a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, Mod a' b')
    AST.Mul es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, Mul (map snd tes'))
    AST.Neg a -> do
      -- TODO: Check type.
      (t, a') <- check_rvalue a
      return (INT, Neg a')
    AST.Not a -> do
      -- TODO: Check type.
      (t, a') <- check_rvalue a
      return (INT, Not a')
    AST.Or es -> do
      -- TODO: Check types.
      tes' <- mapM check_rvalue es
      return (INT, Or (map snd tes'))
    AST.ShiftLeft a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, ShiftLeft a' b')
    AST.ShiftRight a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, ShiftRight a' b')
    AST.Slice r (t, a, b) -> do
      -- TODO: Check that the array type matches the index type (or at least
      -- that cross-type support is present).
      r' <- check_array r
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, Slice r' (t, a', b'))
    AST.Sub a b -> do
      -- TODO: Check types.
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, Sub a' b')
    AST.Variable x -> do
      -- TODO: Check type.
      x' <- find_name x
      case x' of
        Nothing -> do
          print_error loc ("Undefined name " ++ show x)
          return (INT, Name "")
        Just (t, loc') -> do
          case t of
            CONST t' v -> return (t, Value v)
            _ -> return (INT, Name x)

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
        Address a -> do
          v' <- get_static loc a
          case v' of
            WordArray ws ->
              if array_type == AST.INT then
                return (INT, Integer . fromIntegral $ ws !! fromIntegral i)
              else do
                print_error loc "Byte-access to word-arrays is unimplemented."
                return (INT, Integer 0)
            ByteArray bs ->
              if array_type == AST.BYTE then
                return (INT, Integer . fromIntegral . ord $ bs !! fromIntegral i)
              else do
                print_error loc "Word-access to byte-arrays is unimplemented."
                return (INT, Integer 0)
        _ -> type_mismatch
                 loc (if array_type == AST.INT then
                        INT_ARRAY_REF
                      else
                        BYTE_ARRAY_REF) t
    AST.Literal l ->
      case l of
        AST.Bool b -> return (INT, Integer $ if b then true else false)
        AST.Char c -> return (INT, Integer . fromIntegral . ord $ c)
        AST.Integer i -> return (INT, Integer $ value i)
        AST.String s -> do
          -- Store the table in the static blob.
          address <- add_static (ByteArray s)
          -- Return a pointer to the string.
          return (BYTE_ARRAY . fromIntegral $ (length s), Address address)
        AST.Table AST.INT es -> do
          -- Compute the value of the table.
          vs <- mapM check_and_compute_value es
          -- Store the table in the static blob.
          address <- add_static (WordArray vs)
          -- Return a pointer to the table.
          return (INT_ARRAY . fromIntegral $ length vs,
                  Address address)
        AST.Table AST.BYTE es -> do
          -- Compute the value of the table.
          vs <- mapM check_and_compute_value es
          -- Store the table in a static blob.
          let r = map (chr . fromIntegral . (`mod` 256)) vs
          address <- add_static (ByteArray r)
          return (BYTE_ARRAY . fromIntegral $ length vs,
                  Address address)
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
        return (INT_ARRAY_REF, Address 0)
      else if j < 0 then do  -- Check that the slice non-negative.
        print_error loc ("Negative range in slice: " ++ show j ++ ".")
        return (INT_ARRAY_REF, Address 0)
      else
        -- Check that the table and slicer have the same type.
        case v of
          Address a -> do
            v' <- get_static loc a
            case v' of
              WordArray ws -> do
                let ws' = drop (fromIntegral i) ws
                let len = fromIntegral (length ws')
                
                if i > len then do
                  print_error loc "Slice begins past end of array."
                  return (INT_ARRAY 0, Address a)
                else if j > len then do
                  print_error loc "Slice extends past end of array."
                  return (INT_ARRAY len, Address a)
                else
                  return (INT_ARRAY j, Address (a + 4 * i))
              ByteArray bs -> do
                let bs' = drop (fromIntegral i) bs
                let len = fromIntegral (length bs')
                
                if i > len then do
                  print_error loc "Slice begins past end of array."
                  return (BYTE_ARRAY 0, Address a)
                else if j > len then do
                  print_error loc "Slice extends past end of array."
                  return (BYTE_ARRAY len, Address a)
                else
                  return (BYTE_ARRAY j, Address (a + i))
          _ -> if array_type == AST.INT then
                 type_mismatch loc (INT_ARRAY_REF) t
               else
                 type_mismatch loc (BYTE_ARRAY_REF) t
    AST.Sub a b -> do
      x1 <- check_and_compute_value a
      x2 <- check_and_compute_value b
      return (INT, Integer $ val_sub x1 x2)
    AST.Variable n -> do
      result <- find_name n
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
check_and_compute_value :: L AST.Expression -> SemanticAnalyser Int32
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
      x' <- check_name (== CHAN) loc x
      return (Name x')
    AST.Index r (t, i) -> do
      r' <- check_channel_array r
      -- Array access ought to resemble INT indexing.
      if t == AST.INT then
        return ()
      else
        print_error loc "Invalid access to channel array."
      -- TODO: Check type.
      (t', i') <- check_rvalue i
      return (Index r' (t, i'))
    _ -> print_fatal loc "Channel expected."

check_channel_array :: L AST.Expression -> SemanticAnalyser Expression
check_channel_array (L chan_array loc) = do
  -- A channel array is either a named array or a slice.
  case chan_array of
    AST.Variable x -> do
      x' <- check_name (\t ->
        case t of
          CHAN_ARRAY _ -> True
          _ -> False) loc x
      return (Name x')
    AST.Slice r (t, a, b) -> do
      r' <- check_channel_array r
      -- Array access ought to resemble INT slicing.
      if t == AST.INT then
        return ()
      else
        print_error loc "Invalid access to channel array."
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (Slice r' (t, a', b'))
    _ -> print_fatal loc "Channel array expected."

check_lvalue :: L AST.Expression -> SemanticAnalyser (Type, Expression)
check_lvalue (L expr loc) = do
  -- An lvalue is either a variable, an array index, or an array slice.
  -- It can alternatively be ANY, but this is a special case.
  case expr of
    AST.Any -> return (ANY_TYPE, Any)
    AST.Variable x -> do
      x' <- find_name x
      case x' of
        Nothing -> do
          print_error loc ("Undefined name " ++ show x)
          return (INT, Name "")
        Just (t, l) -> do
          let valid_type = case t of
                             BYTE -> True
                             BYTE_ARRAY _ -> True
                             INT -> True
                             INT_ARRAY _ -> True
                             _ -> False
          if not valid_type then do
            print_error loc
                ("Unexpected name " ++ show x ++ " of type " ++ show t ++ ".")
            print_note (show x ++ " is defined at " ++ show l)
            return (INT, Name "")
          else do
            return (t, Name x)
    AST.Index r (t, i) -> do
      -- TODO: Check that access type matches, or at least that cross-type
      -- access support is present.
      r' <- check_array r
      (t', i') <- check_rvalue i
      return (INT, Index r' (t, i'))
    AST.Slice r (t, a, b) -> do
      -- TODO: Check types.
      r' <- check_array r
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (INT, Slice r' (t, a', b'))
    _ -> print_fatal loc "L-value expected."

check_array :: L AST.Expression -> SemanticAnalyser Expression
check_array (L r loc) = do
  -- An array is either a named array or a slice.
  case r of
    AST.Variable x -> do
      x' <- check_name (\t ->
        case t of
          BYTE_ARRAY _ -> True
          INT_ARRAY _ -> True
          _ -> False) loc x
      return (Name x')
    AST.Slice r (t, a, b) -> do
      r' <- check_channel_array r
      -- Array access ought to resemble INT slicing.
      if t == AST.INT then
        return ()
      else
        print_error loc "Invalid access to channel array."
      (t1, a') <- check_rvalue a
      (t2, b') <- check_rvalue b
      return (Slice r' (t, a', b'))
    _ -> print_fatal loc "Array expected."
