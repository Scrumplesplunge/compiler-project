module CodeGen where

import Prelude hiding (EQ, GT)
import AnnotatedAST
import qualified AST
import Code
import Data.List
import Data.Ord (comparing)
import Generator
import Operation

type StackDepth = Integer

-- (space needed, space already taken -> code)
type CodeGenerator = (StackDepth, StackDepth -> Generator Code)

-- Combine the results of two stack computations. If order is 0, the reducer is
-- assumed to be associative. Otherwise, the value should be positive, and this
-- indicates that the order of application should be conserved.
combine :: Code -> Integer -> CodeGenerator -> CodeGenerator -> CodeGenerator
combine c order a b =
  if fst a >= fst b then
    combine' c a b order
  else
    combine' c b a (-order)

conserve_order = 1
ignore_order = 0

-- combine' : Combine two stack computations, where the second requires no more
-- stack space than the first. If reverse is -1/0/1, the order of the components
-- before applying the reducer will be reversed/either/conserved respectively.
combine' c (d1, f1) (d2, f2) reverse =
  if 3 > d1 || (3 >= d1 && d1 > d2) then
    -- Both can be computed within the register stack: no overflow.
    (max d1 (d2 + 1), direct)
  else if d1 > d2 then
    -- At least d1 requires additional space.
    (d1, indirect)
  else  -- d1 == d2
    -- One additional temporary will be required.
    (d1 + 1, indirect)
  where direct d' = do
          -- Perform the computation entirely within the register stack.
          c1 <- f1 d'
          c2 <- f2 d'
          return $ Code [c1, c2, finish reverse]
        indirect d' = do
          -- Perform the computation, making use of a temporary location.
          c1 <- f1 d'
          c2 <- f2 (d' + 1)
          return $ Code [c1, Raw (AJW (-1)), Raw (STL 1), c2,
                         Raw (LDL 1), finish (-reverse), Raw (AJW 1)]
        finish (-1) = Code [Raw REV, c]
        finish _ = c

-- Unary operation.
unop :: Expression -> [Operation] -> CodeGenerator
-- A unary operation uses no additional space, and so cannot possibly overflow
-- the stack.
unop e c = (d, code)
  where (d, f) = gen_expr e
        code d' = do
          e' <- f d'
          return $ Code (e' : map Raw c)

-- Binary operation.
binop :: Expression -> Expression -> [Operation] -> CodeGenerator
-- Calculate the code for both expressions, and also the max depth required to
-- calculate the expression.
binop a b cs =
  combine (Code (map Raw cs)) conserve_order (gen_expr a) (gen_expr b)

-- Associative operation.
assop :: [Expression] -> [Operation] -> CodeGenerator
assop es c = foldl1 (combine (Code $ map Raw c) ignore_order) gs
  where gs = sortBy (comparing fst) $ map gen_expr es

-- Generate code for an expression.
gen_expr :: Expression -> CodeGenerator
gen_expr e =
  case e of
    Add es -> assop es [desc, ADD]
    After a b -> binop b a [desc, DIFF, GT]
    And es -> (maximum (map fst gs), code)
      where gs = map gen_expr es
            code d = do
              -- Short-circuit evaluation of AND.
              end <- label "AND_END"
              cs <- sequence (map (flip ($) d . snd) gs)
              return $ Code (intersperse (Raw (CJ end)) cs ++
                             [Label end, Raw desc])
    Any -> (1, const . return $ Code [Raw desc, Raw (LDC 0)])
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
      (0, const . return $ Code [
          Raw desc, comment "Unimplemented: array indexing."])
    (Mod a b) -> binop a b [desc, REM]
    (Mul es) -> assop es [desc, MUL]
    (Neg e) -> unop e [desc, NOT, ADC 1]
    (Not e) -> unop e [desc, NOT]
    (Or es) -> (maximum (map fst gs), code)
      where gs = map gen_expr es
            code d = do
              -- Short-circuit evaluation of OR.
              end <- label "OR_END"
              cs <- sequence (map (flip ($) d . snd) gs)
              return $ Code (intersperse (Code $ map Raw [NOT, CJ end]) cs ++
                             [Label end, Raw desc])
    (ShiftLeft a b) -> binop a b [desc, SHL]
    (ShiftRight a b) -> binop a b [desc, SHR]
    (Slice a (t, b, c)) ->
      (0, const . return $ comment "Unimplemented: array slicing.")
    (Sub a b) -> binop a b [desc, SUB]
    (Value (Integer v)) ->
      (1, const . return $ Raw (LDC v))
    (Name (Global a) _) ->
      (1, const . return $ Code [Raw (LDC a), Raw (LDNL 0)])
    (Name (Local sl a) _) ->
      -- Local variable addressing must take into account the stack depth.
      (1, \d -> return $ Raw (LDL (a + d)))
  where desc = COMMENT (prettyPrint e)

-- Generate code for an address.
gen_addr :: Expression -> CodeGenerator
gen_addr e =
  case e of
    (Index a (t, b)) ->
      combine index_code conserve_order (gen_addr b) (gen_addr a)
      where index_code = if t == AST.BYTE then Raw ADD else Raw WSUB
    (Slice a (t, b, c)) ->
      (0, const . return $ comment "Unimplemented: slice assignment.")
    (Name loc x) ->
      case loc of
        Global a -> (1, const . return $ Raw (LDC a))
        Local sl a -> (1, \d -> return $ Raw (LDLP (a + d)))
    _ -> error "Generating address for non-assignable type."

-- Generate code for a sequence.
gen_seq :: Replicable Process -> CodeGenerator
gen_seq rp =
  case rp of
    Basic as -> (maximum (map fst gs), code)
      where gs = map gen_proc as
            code d = do
              cs <- sequence (map (flip ($) d . snd) gs)
              return $ Code cs
    Replicated (Range i a b) p -> (maximum [da, db, dp] + 2, code)
      where (da, ga) = gen_expr a
            (db, gb) = gen_expr b
            (dp, gp) = gen_proc p
            code d = do
              ca <- ga (d + 2)
              cb <- gb (d + 2)
              cp <- gp (d + 2)
              loop <- label "REP_SEQ"
              return $ Code [desc, Raw (AJW (-2)), ca, Raw (STL 1), cb,
                             Raw (STL 2), Label loop, cp, Raw (LDLP 1),
                             Raw (LEND loop), Raw (AJW 2)]
  where desc = comment $ prettyPrint (Seq rp)

-- Generate code for a process.
gen_proc :: Process -> CodeGenerator
gen_proc p =
  case p of
    Assign a b -> combine (Code [desc, Raw (STNL 0)]) conserve_order
                          (gen_expr b) (gen_addr a)
    Seq a -> gen_seq a
    Skip -> (0, const . return $ comment "SKIP")
    Stop -> (0, const . return $ Raw STOPP)
    While e p -> (max de dp, code)
      where (de, ge) = gen_expr e
            (dp, gp) = gen_proc p
            code d = do
              while_start <- label "WHILE_START"
              ce <- ge d
              cp <- gp d
              while_end <- label "WHILE_END"
              -- An alternative construct for the WHILE loop would place the
              -- condition check at the bottom, which saves one instruction per
              -- iteration (specifically, a conditional jump that does not
              -- activate). However, this would make the loop atomic, which
              -- would be detrimental to the task scheduling of the transputer.
              return $ Code [desc, Label while_start, ce, Raw (CJ while_end),
                             cp, Raw (J while_start), Label while_end]
    _ -> error "Unimplemented process."
  where desc = comment (prettyPrint p)

-- Run a code generator and output the generated code.
assemble :: StackDepth -> Process -> IO ()
assemble initial_depth p = do
  let (max_depth, gp) = gen_proc p
  let code = run_generator (gp initial_depth)
  putStrLn . showCode $ Code [comment ("Max additional depth: " ++ show max_depth), code]
