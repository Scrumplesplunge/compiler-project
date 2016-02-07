module CodeGen where

import Prelude hiding (EQ, GT)
import AnnotatedAST
import Code
import Data.List
import Generator
import Operation

type StackDepth = Integer

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
  (d1, a') <- gen_expr a
  (d2, b') <- gen_expr b
  if d1 > d2 then
    -- Both fit in the existing space: no overflow.
    return (d1, Code (a' : b' : map Raw cs))
  else if d2 > d1 then
    -- Both fit in the existing space: no overflow.
    return (d2, Code (b' : a' : map Raw (REV : cs)))
  else if d1 < 3 then
    -- The space required does not exceed the register stack: no overflow.
    return (d1 + 1, Code (a' : b' : map Raw cs))
  else
    -- The space required exceeds the register stack. Use a temporary variable.
    return (3, Code ([Comment "Binary operation exceeds register stack.",
                      a', Raw (AJW (-1)), Raw (STL 1), b'] ++
                     map Raw (LDL 1 : REV : cs) ++ [Raw (AJW 1)]))

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
    return (3, Code [Comment "Associative operation exceeds register stack.",
                     snd a, Raw (AJW (-1)), large_code_tail, small_code_tail,
                     Raw (AJW 1)])

-- Generate code for an expression.
gen_expr :: Expression -> Generator (Integer, Code)
gen_expr (Add es) = assop es [ADD]
gen_expr (After a b) = binop b a [DIFF, GT]
gen_expr (And es) = do
  es' <- mapM gen_expr es
  end <- label "AND_END"
  return (maximum (map fst es'),
          Code (intersperse (Raw (CJ end)) (map snd es') ++ [Label end]))
gen_expr Any = return (1, Raw (LDC 0))
gen_expr (BitwiseAnd es) = assop es [AND]
gen_expr (BitwiseOr es) = assop es [OR]
gen_expr (BitwiseXor es) = assop es [XOR]
gen_expr (CompareEQ a b) = binop a b [EQ]
gen_expr (CompareLE a b) = binop a b [GT, NOT]
gen_expr (CompareLT a b) = binop b a [GT]
gen_expr (CompareGE a b) = binop b a [GT, NOT]
gen_expr (CompareGT a b) = binop a b [GT]
gen_expr (CompareNE a b) = binop a b [EQ, NOT]
gen_expr (Div a b) = binop a b [DIV]
gen_expr (Index a (t, b)) = return (0, Raw (UNIMPLEMENTED "Array indexing"))
gen_expr (Mod a b) = binop a b [REM]
gen_expr (Mul es) = assop es [MUL]
gen_expr (Neg e) = unop e [NOT, ADC 1]
gen_expr (Not e) = unop e [NOT]
gen_expr (Or es) = do
  es' <- mapM gen_expr es
  end <- label "OR_END"
  return (maximum (map fst es'),
          Code (intersperse (Code $ map Raw [NOT, CJ end]) (map snd es') ++
          [Label end, Raw NOT]))
gen_expr (ShiftLeft a b) = binop a b [SHL]
gen_expr (ShiftRight a b) = binop a b [SHR]
gen_expr (Slice a (t, b, c)) = return (0, Raw (UNIMPLEMENTED "Array slicing"))
gen_expr (Sub a b) = binop a b [SUB]
gen_expr (Value (Integer v)) = return (1, Raw (LDC v))
gen_expr (Name (Global a) _) = return (1, Code [Raw (LDC a), Raw (LDNL 0)])
gen_expr (Name (Local a) _) = return (1, Raw (LDL a))

-- Run a code generator and output the generated code.
assemble :: Expression -> IO ()
assemble e = do
  let (depth, code) = run_generator (gen_expr e)
  putStrLn . showCode $ code
