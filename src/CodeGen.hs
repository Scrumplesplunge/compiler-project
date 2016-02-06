module CodeGen where

import AnnotatedAST
import Code
import Data.List
import Operation

-- Unary operation.
unop :: Expression -> [Operation] -> Generator Code
unop e c = do
  e' <- gen_expr e
  return $ Code (e' : map Raw c)

-- Binary operation.
binop :: Expression -> Expression -> [Operation] -> Generator Code
binop a b c = do
  a' <- gen_expr a
  b' <- gen_expr b
  return $ Code (a' : b' : map Raw c)

-- Associative operation.
assop :: [Expression] -> [Operation] -> Generator Code
assop es c = do
  es' <- mapM gen_expr es
  let first = head es'
  let rest = map (\e' -> Code (e' : map Raw c)) $ tail es'
  return $ Code (first : rest)

-- Generate code for an expression.
gen_expr :: Expression -> Generator Code
gen_expr (Add es) = assop es [ADD]
gen_expr (After a b) = binop a b [AFTER]
gen_expr (And es) = do
  es' <- mapM gen_expr es
  end <- label "AND_END"
  return $ Code (intersperse (Raw (COND_JUMP end)) es' ++ [Label end])
gen_expr Any = return $ Raw (CONSTANT 0)
gen_expr (BitwiseAnd es) = assop es [BITWISE_AND]
gen_expr (BitwiseOr es) = assop es [BITWISE_OR]
gen_expr (BitwiseXor es) = assop es [BITWISE_XOR]
gen_expr (CompareEQ a b) = binop a b [COMPARE_EQ]
gen_expr (CompareLE a b) = binop a b [COMPARE_GT, LOGICAL_NOT]
gen_expr (CompareLT a b) = binop b a [COMPARE_GT]
gen_expr (CompareGE a b) = binop b a [COMPARE_GT, LOGICAL_NOT]
gen_expr (CompareGT a b) = binop a b [COMPARE_GT]
gen_expr (CompareNE a b) = binop a b [COMPARE_EQ, LOGICAL_NOT]
gen_expr (Div a b) = binop a b [DIV]
gen_expr (Index a (t, b)) = return . Raw $ UNIMPLEMENTED "Array indexing"
gen_expr (Mod a b) = binop a b [MOD]
gen_expr (Mul es) = assop es [MUL]
gen_expr (Neg e) = unop e [NEG]
gen_expr (Not e) = unop e [LOGICAL_NOT]
gen_expr (Or es) = do
  es' <- mapM gen_expr es
  end <- label "OR_END"
  return $ Code (intersperse (Raw (COND_JUMP end)) es' ++ [Label end])
gen_expr (ShiftLeft a b) = binop a b [SHIFT_LEFT]
gen_expr (ShiftRight a b) = binop a b [SHIFT_RIGHT]
gen_expr (Slice a (t, b, c)) = return . Raw $ UNIMPLEMENTED "Array slicing"
gen_expr (Sub a b) = binop a b [SUB]
gen_expr (Value (Integer v)) = return $ Raw (CONSTANT v)
gen_expr (Name (Global a) _) =
  return $ Code [Raw (CONSTANT a), Raw (LOAD_NON_LOCAL 0)]
gen_expr (Name (Local a) _) = return $ Raw (LOAD_LOCAL a)

-- Run a code generator and output the generated code.
assemble :: Expression -> IO ()
assemble e = do
  let code = run_generator (gen_expr e)
  putStrLn . showCode $ code
