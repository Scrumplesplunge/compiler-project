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
    return (3, Code ([comment "Binary operation exceeds register stack.",
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
    (Index a (t, b)) -> return (0, Code [Raw desc, comment "Unimplemented: array indexing"])
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
    (Slice a (t, b, c)) -> return (0, comment "Unimplemented: array slicing")
    (Sub a b) -> binop a b [desc, SUB]
    (Value (Integer v)) -> return (1, Raw (LDC v))
    (Name (Global a) _) -> return (1, Code [Raw (LDC a), Raw (LDNL 0)])
    (Name (Local a) _) -> return (1, Raw (LDL a))
  where desc = COMMENT (prettyPrint e)

-- Run a code generator and output the generated code.
assemble :: Expression -> IO ()
assemble e = do
  let (depth, code) = run_generator (gen_expr e)
  putStrLn . showCode $ code
