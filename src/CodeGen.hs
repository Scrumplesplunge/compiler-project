module CodeGen where

import Prelude hiding (EQ, GT)
import AnnotatedAST
import qualified AST
import Code
import Data.List
import Generator
import Operation

type StackDepth = Integer

-- Combine the results of two stack computations.
combine :: (StackDepth, Code) -> (StackDepth, Code) -> Code
        -> (StackDepth, Code)
combine (d1, a) (d2, b) c =
  if d1 > d2 then
    -- Both fit in the existing space: no overflow.
    (d1, Code [a, b, c])
  else if d2 > d1 then
    -- Both fit in the existing space: no overflow.
    (d2, Code [b, a, Raw REV, c])
  else if d1 < 3 then
    -- The space required does not exceed the register stack: no overflow.
    (d1 + 1, Code [a, b, c])
  else
    -- The space required exceeds the register stack. Use a temporary variable.
    (3, Code [comment "Operation exceeds register stack.",
              b, Raw (AJW (-1)), Raw (STL 1), a, Raw (LDL 1), c, Raw (AJW 1)])

-- Unary operation.
unop :: Expression -> [Operation] -> Generator (StackDepth, Code)
unop e c = do
  -- A unary operation uses no additional space, and so cannot possibly overflow
  -- the stack.
  (d, e') <- gen_expr e
  return (d, Code (e' : map Raw c))

-- Binary operation.
binop :: Expression -> Expression -> [Operation] -> Generator (StackDepth, Code)
binop a b cs = do
  -- Calculate the code for both expressions, and also the max depth required to
  -- calculate the expression.
  a' <- gen_expr a
  b' <- gen_expr b
  return (combine a' b' (Code (map Raw cs)))

-- Associative operation.
assop :: [Expression] -> [Operation] -> Generator (StackDepth, Code)
assop es c = do
  des' <- mapM gen_expr es
  -- Find the deepest and second deepest expressions, in that order.
  let find_deepest (a : b : xs) = f (p, q, []) xs
        where p = if fst a < fst b then b else a
              q = if fst a < fst b then a else b
              f y [] = y
              f (a, b, ys) (x : xs) =
                if fst x > fst a then
                  f (x, a, b : ys) xs  -- Deepest so far.
                else if fst x > fst b then
                  f (a, x, b : ys) xs  -- Second deepest so far.
                else
                  f (a, b, x : ys) xs  -- Neither.
  let (a, b, rest) = find_deepest des'
  -- If fst a > fst b, then a is strictly the deepest expression. Thus,
  -- calculating any other expression afterwards (after taking into account the
  -- buffer space for the result) cannot exceed depth fst a. If fst a = fst b,
  -- then one more slot will be required in order to calculate any other
  -- expression which itself requires fst a slots.
  let op = Code (map Raw c)
  let code_tail = Code (map (\(_, e) -> Code [e, op]) (b : rest))
  if fst a > fst b then
    -- All fit within the existing space: no overflow.
    return (fst a, Code [snd a, code_tail])
  else if fst a < 3 then
    -- Space required does not exceed the register stack: no overflow.
    return (fst a + 1, Code [snd a, code_tail])
  else
    -- fst a = fst b = 3. The space required exceeds the register stack. Use a
    -- temporary variable as the accumulator until all the large expressions are
    -- dealt with. After that, there is enough space in the register stack.
    let (large, small) = partition ((3 ==) . fst) (b : rest) in
    let large_code_tail =
          Code (map (\x -> Code [Raw (STL 1), snd x, Raw (LDL 1), op]) large) in
    let small_code_tail =
          Code (map (\x -> Code [snd x, op]) small) in
    return (3, Code [comment "Associative operation exceeds register stack.",
                     snd a, Raw (AJW (-1)), large_code_tail, small_code_tail,
                     Raw (AJW 1)])

-- Generate code for an expression.
gen_expr :: Expression -> Generator (Integer, Code)
gen_expr e =
  case e of
    Add es -> assop es [desc, ADD]
    After a b -> binop b a [desc, DIFF, GT]
    And es -> do
      es' <- mapM gen_expr es
      end <- label "AND_END"
      return (maximum (map fst es'),
              Code (intersperse (Raw (CJ end)) (map snd es') ++
              [Label end, Raw desc]))
    Any -> return (1, Code [Raw desc, Raw (LDC 0)])
    (BitwiseAnd es) -> assop es [desc, AND]
    (BitwiseOr es) -> assop es [desc, OR]
    (BitwiseXor es) -> assop es [desc, XOR]
    (CompareEQ a b) -> binop a b [desc, EQ]
    (CompareLE a b) -> binop a b [desc, GT, NOT]
    (CompareLT a b) -> binop b a [desc, GT]
    (CompareGE a b) -> binop b a [desc, GT, NOT]
    (CompareGT a b) -> binop a b [desc, GT]
    (CompareNE a b) -> binop a b [desc, EQ, NOT]
    (Div a b) -> binop a b [desc, DIV]
    (Index a (t, b)) ->
      return (0, Code [Raw desc, comment "Unimplemented: array indexing."])
    (Mod a b) -> binop a b [desc, REM]
    (Mul es) -> assop es [desc, MUL]
    (Neg e) -> unop e [desc, NOT, ADC 1]
    (Not e) -> unop e [desc, NOT]
    (Or es) -> do
      es' <- mapM gen_expr es
      end <- label "OR_END"
      return (maximum (map fst es'),
              Code (intersperse (Code $ map Raw [NOT, CJ end]) (map snd es') ++
                    [Label end, Raw NOT, Raw desc]))
    (ShiftLeft a b) -> binop a b [desc, SHL]
    (ShiftRight a b) -> binop a b [desc, SHR]
    (Slice a (t, b, c)) -> return (0, comment "Unimplemented: array slicing.")
    (Sub a b) -> binop a b [desc, SUB]
    (Value (Integer v)) -> return (1, Raw (LDC v))
    (Name (Global a) _) -> return (1, Code [Raw (LDC a), Raw (LDNL 0)])
    (Name (Local a) _) -> return (1, Raw (LDL a))
  where desc = COMMENT (prettyPrint e)

-- Generate code for an address.
gen_addr :: Expression -> Generator (StackDepth, Code)
gen_addr e =
  case e of
    (Index a (t, b)) -> do
      a' <- gen_addr a
      b' <- gen_expr b
      let index_code = if t == AST.BYTE then Raw ADD else Raw WSUB
      return (combine b' a' index_code)
    (Slice a (t, b, c)) ->
      return (0, comment "Unimplemented: slice assignment.")
    (Name loc x) ->
      case loc of
        Global a -> return (1, Raw (LDC a))
        Local a -> return (1, Raw (LDLP a))
    _ -> error "Generating address for non-assignable type."

-- Generate code for a sequence.
gen_seq :: Replicable Process -> Generator (StackDepth, Code)
gen_seq rp =
  case rp of
    Basic as -> do
      as' <- mapM gen_proc as
      return (maximum (map fst as'),
              Code (desc : map snd as'))
    Replicated (Range i a b) p -> do
      (da, a') <- gen_expr a
      (db, b') <- gen_expr b
      (dp, p') <- gen_proc p
      loop <- label "LOOP"
      return (maximum [da, db, dp],
              Code [desc, Raw (AJW (-2)), a', Raw (STL 1), b', Raw (STL 2),
                    Label loop, p', Raw (LDLP 1), Raw LEND])
  where desc = comment $ prettyPrint (Seq rp)

-- Generate code for a process.
gen_proc :: Process -> Generator (StackDepth, Code)
gen_proc p =
  case p of
    Assign a b -> do
      a' <- gen_addr a
      b' <- gen_expr b
      return $ combine b' a' (Code [desc, Raw (STNL 0)])
    Seq a -> gen_seq a
    Skip -> return (0, comment "SKIP")
    Stop -> return (0, Raw STOPP)
    While e p -> do
      (d1, e') <- gen_expr e
      (d2, p') <- gen_proc p
      while_start <- label "WHILE_START"
      while_end <- label "WHILE_END"
      return (max d1 d2, Code [desc, Label while_start, e', Raw (CJ while_end),
                               p', Label while_end])
    _ -> error "Unimplemented process."
  where desc = comment (prettyPrint p)

-- Run a code generator and output the generated code.
assemble :: Process -> IO ()
assemble p = do
  let (depth, code) = run_generator (gen_proc p)
  putStrLn . showCode $ code
