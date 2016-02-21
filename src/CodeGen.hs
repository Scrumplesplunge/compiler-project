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

data Promise = Promise {
  depth_required :: StackDepth
}

-- Base (minimal) promise.
promise = Promise {
  depth_required = 0
}

type Environment = [(Name, Allocation)]

data Context = Context {
  static_level :: Integer,
  stack_depth :: StackDepth,
  environment :: Environment
}

-- Base (minimal) context.
context = Context {
  static_level = 0,
  stack_depth = 0,
  environment = []
}

-- Allocate a variable in the context.
allocate :: Context -> Name -> Integer -> Context
allocate ctx x size = ctx { stack_depth = pos,
                            environment = (x, a) : environment ctx }
  where pos = stack_depth ctx + size
        a = Local (static_level ctx) pos

-- Initialize a local variable.
initialize :: AST.RawType -> Integer -> Code
initialize t id =
  case t of
    AST.CHAN ->
      Raw [MINT, STL id]
    AST.VALUE ->
      Raw [LDC 0, STL id]
    _ -> error "Don't know how to initialize this."

-- (space needed, space already taken -> code)
type CodeGenerator = (Promise, Context -> Generator Code)

-- Combine the results of two stack computations. If order is 0, the reducer is
-- assumed to be associative. Otherwise, the value should be positive, and this
-- indicates that the order of application should be conserved.
combine :: Code -> Integer -> CodeGenerator -> CodeGenerator -> CodeGenerator
combine c order a b =
  if depth_required (fst a) >= depth_required (fst b) then
    combine' c order a b
  else
    combine' c (-order) b a

conserve_order = 1
ignore_order = 0

-- combine' : Combine two stack computations, where the second requires no more
-- stack space than the first. If reverse is -1/0/1, the order of the components
-- before applying the reducer will be reversed/either/conserved respectively.
combine' :: Code -> Integer -> CodeGenerator -> CodeGenerator -> CodeGenerator
combine' c reverse (p1, g1) (p2, g2) =
  if 3 > d1 || (3 >= d1 && d1 > d2) then
    -- Both can be computed within the register stack: no overflow.
    (promise { depth_required = max d1 (d2 + 1) }, direct)
  else if d1 > d2 then
    -- At least d1 requires additional space.
    (promise { depth_required = d1 }, indirect)
  else  -- d1 == d2
    -- One additional temporary will be required.
    (promise { depth_required = d1 + 1 }, indirect)
  where d1 = depth_required p1
        d2 = depth_required p2
        direct :: Context -> Generator Code
        direct ctx = do
          -- Perform the computation entirely within the register stack.
          c1 <- g1 ctx
          c2 <- g2 ctx
          return $ Code [c1, c2, finish reverse]
        indirect :: Context -> Generator Code
        indirect ctx = do
          -- Perform the computation, making use of a temporary location.
          c1 <- g1 ctx
          c2 <- g2 (ctx { stack_depth = stack_depth ctx + 1 })
          return $ Code [c1, Raw [AJW (-1), STL 1], c2,
                         Raw [LDL 1], finish (-reverse), Raw [AJW 1]]
        finish (-1) = Code [Raw [REV], c]
        finish _ = c

-- Unary operation.
unop :: Expression -> [Operation] -> CodeGenerator
-- A unary operation uses no additional space, and so cannot possibly overflow
-- the stack.
unop e os = (p, code)
  where (p, g) = gen_expr e
        code ctx = do
          c <- g ctx
          return $ Code [c, Raw os]

-- Binary operation.
binop :: Expression -> Expression -> [Operation] -> CodeGenerator
-- Calculate the code for both expressions, and also the max depth required to
-- calculate the expression.
binop a b cs = combine (Raw cs) conserve_order (gen_expr a) (gen_expr b)

-- Associative operation.
assop :: [Expression] -> [Operation] -> CodeGenerator
assop es c = foldl1 (combine (Raw c) ignore_order) gs
  where gs = sortBy (comparing (depth_required . fst)) $ map gen_expr es

-- Generate code for an expression.
gen_expr :: Expression -> CodeGenerator
gen_expr e =
  case e of
    Add es -> assop es [desc, ADD]
    After a b -> binop b a [desc, DIFF, GT]
    And es -> (p, code)
      where gs = map gen_expr es
            p = promise { depth_required = maximum (map (depth_required . fst) gs) }
            code ctx = do
              -- Short-circuit evaluation of AND.
              end <- label "AND_END"
              cs <- sequence (map (($ ctx) . snd) gs)
              return $ Code (intersperse (Raw [CJ end]) cs ++
                             [Label end, Raw [desc]])
    Any -> (promise { depth_required = 1 }, \ctx -> return $ Raw [desc, LDC 0])
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
    (Index a (t, b)) -> (p, code)
      where (p, ge) = gen_addr e
            code ctx = do
              ce <- ge ctx
              return $ Code [ce, Raw [LDNL 0]]
    (Mod a b) -> binop a b [desc, REM]
    (Mul es) -> assop es [desc, MUL]
    (Neg e) -> unop e [desc, NOT, ADC 1]
    (Not e) -> unop e [desc, NOT]
    (Or es) -> (p, code)
      where gs = map gen_expr es
            p = promise { depth_required = maximum (map (depth_required . fst) gs) }
            code ctx = do
              -- Short-circuit evaluation of OR.
              end <- label "OR_END"
              cs <- sequence (map (($ ctx) . snd) gs)
              return $ Code (intersperse (Raw [NOT, CJ end]) cs ++
                             [Label end, Raw [desc]])
    (ShiftLeft a b) -> binop a b [desc, SHL]
    (ShiftRight a b) -> binop a b [desc, SHR]
    (Slice a (t, b, c)) ->
      (promise, \ctx -> return $ comment "Unimplemented: array slicing.")
    (Sub a b) -> binop a b [desc, SUB]
    (Value (Integer v)) ->
      (promise { depth_required = 1 }, \ctx -> return $ Raw [LDC v])
    (Name x) -> (promise { depth_required = 1 }, code)
      where code ctx =
              case lookup x (environment ctx) of
                Nothing -> error "Logic error: lookup failed at code gen."
                Just (Global a) -> return (load_global ctx a)
                Just (Local sl off) ->
                  return (load_local ctx (static_level ctx - sl) off)
            load_global ctx a = Raw [LDC a, LDNL 0]
            load_local ctx sl_diff off =
              if sl_diff == 0 then
                -- We are already at the same static level. Use load-local.
                Raw [LDL (stack_depth ctx - off + 1)]
              else
                -- Need to descend a few static levels. Use non-local.
                Raw ([LDL (stack_depth ctx)] ++
                     replicate (fromInteger $ sl_diff - 1) (LDNL 0) ++
                     [LDNL (-off)])
  where desc = COMMENT (prettyPrint e)

-- Generate code for an address.
gen_addr :: Expression -> CodeGenerator
gen_addr e =
  case e of
    (Index a (t, b)) ->
      combine index_code conserve_order (gen_expr b) (gen_addr a)
      where index_code = if t == AST.BYTE then Raw [ADD] else Raw [WSUB]
    (Slice a (t, b, c)) ->
      (promise, const . return $ comment "Unimplemented: slice assignment.")
    (Name x) -> (promise { depth_required = 1 }, code)
      where code ctx =
              case lookup x (environment ctx) of
                Nothing -> error "Logic error: lookup failed at code gen."
                Just (Global a) -> return (load_global_pointer ctx a)
                Just (Local sl off) ->
                  return (load_local_pointer ctx (static_level ctx - sl) off)
            load_global_pointer ctx a = Raw [LDC a]
            load_local_pointer ctx sl_diff off =
              if sl_diff == 0 then
                -- We are already at the same static level. Use load-local.
                Raw [LDLP (stack_depth ctx - off + 1)]
              else
                -- Need to descend a few static levels. Use non-local.
                Raw ([LDL (stack_depth ctx)] ++
                     replicate (fromInteger $ sl_diff - 1) (LDNL 0) ++
                     [LDNLP (-off)])
    _ -> error "Generating address for non-assignable type."

-- Generate code for a sequence.
gen_seq :: Replicable Process -> CodeGenerator
gen_seq rp =
  case rp of
    Basic as -> (promise { depth_required = d }, code)
      where d = maximum (map (depth_required . fst) gs)
            gs = map gen_proc as
            code ctx = do
              cs <- sequence (map (flip ($) ctx . snd) gs)
              return $ Code [desc, Code cs]
    Replicated (Range i a b) p -> (promise { depth_required = depth }, code)
      where (pa, ga) = gen_expr a
            (pb, gb) = gen_expr b
            (pp, gp) = gen_proc p
            depth = maximum (map depth_required [pa, pb, pp]) + 2
            code ctx = do
              let ctx' = allocate ctx i 2
              ca <- ga ctx'
              cb <- gb ctx'
              cp <- gp ctx'
              loop <- label "REP_SEQ"
              return $ Code [desc, Raw [AJW (-2)], ca, Raw [STL 1], cb,
                             Raw [STL 2], Label loop, cp, Raw [LDLP 1],
                             Raw [LEND loop, AJW 2]]
  where desc = comment $ prettyPrint (Seq rp)

-- Generate code for a process.
gen_proc :: Process -> CodeGenerator
gen_proc p =
  case p of
    Assign a b -> combine (Code [desc, Raw [STNL 0]]) conserve_order
                          (gen_expr b) (gen_addr a)
    Define t x size proc -> (p, code)
      where (p, gp) = gen_proc proc
            code ctx = do
              let ctx' = allocate ctx x size
              cp <- gp ctx'
              return $ Code [desc, Raw [AJW (-size)],
                             Code $ map (initialize t) [1..size], cp,
                             desc, Raw [AJW size]]
    DefineConstant x v proc -> (p, code)
      where (p, gp) = gen_proc proc
            code ctx = do
              let alloc =
                    case v of
                      Integer i -> AnnotatedAST.Constant i
                      Address a -> Global a
              let ctx' = ctx { environment = (x, alloc) : environment ctx }
              cp <- gp ctx'
              return cp
    Seq a -> gen_seq a
    Skip -> (promise, \ctx -> return $ comment "SKIP")
    Stop -> (promise, \ctx -> return $ Raw [STOPP])
    While e p -> (promise { depth_required = depth }, code)
      where (pe, ge) = gen_expr e
            (pp, gp) = gen_proc p
            depth = max (depth_required pe) (depth_required pp)
            code ctx = do
              while_start <- label "WHILE_START"
              ce <- ge ctx
              cp <- gp ctx
              while_end <- label "WHILE_END"
              -- An alternative construct for the WHILE loop would place the
              -- condition check at the bottom, which saves one instruction per
              -- iteration (specifically, a conditional jump that does not
              -- activate). However, this would make the loop atomic, which
              -- would be detrimental to the task scheduling of the transputer.
              return $ Code [desc, Label while_start, ce, Raw [CJ while_end],
                             cp, Raw [J while_start], Label while_end]
    _ -> error "Unimplemented process."
  where desc = comment (prettyPrint p)

-- Run a code generator and output the generated code.
assemble :: Context -> Process -> IO ()
assemble ctx process = do
  let (promise, gp) = gen_proc process
  let text = run_generator (do
        code <- gp ctx
        showCode $ Code [
          comment ("Max additional depth: " ++ show (depth_required promise)),
          code])
  putStrLn text
