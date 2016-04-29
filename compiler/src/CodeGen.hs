module CodeGen where

import Prelude hiding (EQ, GT)
import AnnotatedAST
import qualified AST
import Code
import Data.List
import Data.Ord (comparing)
import Generator
import Operation
import System.IO

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
  static_level :: Integer,    -- Current static link level.
  stack_depth :: StackDepth,  -- Stack depth in current link level.
  environment :: Environment, -- Defined variables.
  if_exit_label :: String,    -- Exit location for the enclosing IF statement.
  alt_end_label :: String,    -- Location of ALTEND for the enclosing ALT.
  alt_exit_label :: String    -- Exit location for the enclosing ALT statement.
}

-- Base (minimal) context.
context = Context {
  static_level = 0,
  stack_depth = 0,
  environment = [],
  if_exit_label = "<if_exit_label undefined>",
  alt_end_label = "<alt_end_label undefined>",
  alt_exit_label = "<alt_exit_label undefined>"
}

new_static_level :: Context -> Context
new_static_level ctx = ctx { static_level = static_level ctx + 1,
                             stack_depth = 1 }

-- Allocate a variable in the context.
allocate :: Context -> Name -> Integer -> Context
allocate ctx x size = ctx { stack_depth = pos,
                            environment = (x, a) : environment ctx }
  where pos = stack_depth ctx + size
        a = Local (static_level ctx) pos

-- Construct a (non-zero) number of local variables. The variables are assumed
-- to start at local 1.
initialize :: AST.RawType -> Integer -> Generator Code
initialize t size = do
  case t of
    -- Don't initialize VARs.
    AST.VALUE -> return $ Raw []
    -- Initialize CHANs.
    AST.CHAN ->
      if size <= 5 then
        -- Initialize directly.
        return $ Code (map (\x -> Raw [LDLP x, RESETCH]) [1..size])
      else do
        -- Initialize all except the first two using a loop.
        repeat <- label "INIT"
        return $ Code [Raw [LDC 3, STL 1, LDC size, STL 2], Label repeat,
                       Raw [LDL 1, LDLP 0, WSUB, RESETCH, LDLP 1, LEND repeat,
                            LDLP 1, RESETCH, LDLP 2, RESETCH]]
    _ -> error $ "Don't know how to initialize " ++ show t

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

-- Symmetric (commutative) operation.
symop :: [Expression] -> [Operation] -> CodeGenerator
symop es c = foldl1 (combine (Raw c) ignore_order) gs
  where gs = sortBy (comparing (depth_required . fst)) $ map gen_expr es

-- Generate code for an expression.
gen_expr :: Expression -> CodeGenerator
gen_expr e =
  case e of
    Add es -> symop es [desc, ADD]
    After a b -> binop b a [desc, DIFF, GT]
    And es -> (p, code)
      where gs = map gen_expr es
            p = promise {
              depth_required = maximum (map (depth_required . fst) gs)
            }
            code ctx = do
              -- Short-circuit evaluation of AND.
              end <- label "AND_END"
              cs <- sequence (map (($ ctx) . snd) gs)
              return $ Code (intersperse (Raw [CJ end]) cs ++
                             [Label end, Raw [desc]])
    Any -> (promise { depth_required = 1 }, \ctx -> return $ Raw [desc, LDC 0])
    (BitwiseAnd es) -> symop es [desc, AND]
    (BitwiseOr es) -> symop es [desc, OR]
    (BitwiseXor es) -> symop es [desc, XOR]
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
              return $ Code [ce, Raw [if t == AST.BYTE then LB else LDNL 0]]
    (Mod a b) -> binop a b [desc, REM]
    (Mul es) -> symop es [desc, MUL]
    (Neg e) -> unop e [desc, NOT, ADC 1]
    (Not e) -> unop e [desc, NOT]
    (Or es) -> (p, code)
      where gs = map gen_expr es
            p = promise {
              depth_required = maximum (map (depth_required . fst) gs)
            }
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
                Raw ([COMMENT (show (environment ctx)),
                      LDL (stack_depth ctx)] ++
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
            load_global_pointer ctx a = Raw [MINT, ADC (a - two_pow_31)]
            load_local_pointer ctx sl_diff off =
              if sl_diff == 0 then
                -- We are already at the same static level. Use load-local.
                Raw [LDLP (stack_depth ctx - off + 1)]
              else
                -- Need to descend a few static levels. Use non-local.
                Raw ([COMMENT (show (environment ctx)),
                      LDL (stack_depth ctx)] ++
                     replicate (fromInteger $ sl_diff - 1) (LDNL 0) ++
                     [LDNLP (-off)])
    Any -> (promise { depth_required = 1 }, code)
      where code ctx = return $ Raw [LDLP 0]
    _ -> error ("Generating address for non-assignable type: " ++ show e)

-- Generate code for a single branch of a conditional, with the given exit
-- label.
gen_branch :: StackDepth -> Expression -> Process -> CodeGenerator
gen_branch depth guard proc = (promise { depth_required = d }, code)
  where (pg, gg) = gen_expr guard
        (pp, gp) = gen_proc proc
        desc = comment $ "IF " ++ prettyPrint guard
        d = maximum (map depth_required [pg, pp])
        code ctx = do
          cg <- gg ctx
          cp <- gp ctx
          end_branch <- label "END_BRANCH"
          return $ Code [desc,
                         cg,
                         Raw [CJ end_branch],
                         cp,
                         -- Deallocate the replicators before exiting.
                         Raw [AJW depth, J (if_exit_label ctx)],
                         Label end_branch]

-- Generate code for a nestable conditional, with the given exit label.
gen_nestable_branch :: StackDepth -> Nestable Condition Expression
                    -> CodeGenerator
gen_nestable_branch depth (Nested cond) = gen_cond_exit depth cond
gen_nestable_branch depth (Block guard proc) = gen_branch depth guard proc

-- Generate code for a conditional block, jumping to the given label at the end
-- of the selected branch.
gen_cond_exit :: StackDepth -> Condition -> CodeGenerator
gen_cond_exit depth (Condition rnc) =
  case rnc of
    Basic ncs -> (promise { depth_required = d }, code)
      where pgcs = map (gen_nestable_branch depth) ncs
            d = maximum (map (depth_required . fst) pgcs)
            code ctx = do
              ccs <- mapM (($ ctx) . snd) pgcs
              return $ Code ccs
    Replicated (Range i a b) nc -> (promise { depth_required = d }, code)
      where (pa, ga) = gen_expr a
            (pb, gb) = gen_expr b
            -- If a branch is selected from this part, it will have to
            -- deallocate this replicator.
            (pc, gc) = gen_nestable_branch (depth + 2) nc
            d = maximum (map depth_required [pa, pb, pc]) + 2
            code ctx = do
              let ctx' = allocate ctx i 2
              ca <- ga ctx'
              cb <- gb ctx'
              cc <- gc ctx'
              loop <- label "REP_IF"
              return $ Code [Raw [AJW (-2)], ca, Raw [STL 1], cb, Raw [STL 2],
                             Label loop, cc, Raw [LDLP 1],
                             Raw [LEND loop, AJW 2]]

-- Generate code for a conditional block.
gen_cond :: Condition -> CodeGenerator
gen_cond cond = (pc, code)
  where (pc, gc) = gen_cond_exit 0 cond
        code ctx = do
          exit_label <- label "END_IF"
          let ctx' = ctx { if_exit_label = exit_label }
          cc <- gc ctx'
          return $ Code [desc,
                         cc,
                         Raw [STOPP],
                         Label exit_label]
        desc = comment $ prettyPrint cond

-- When the disable sequences are generated, the code for each service is also
-- generated.
type DisableGenerator = (Promise, Context -> Generator (Code, Code))

-- Generate code for a single guarded branch of an alternative, with the given
-- exit label.
gen_guard_enable :: Guard -> CodeGenerator
gen_guard_enable guard =
  case guard of
    BasicGuard atomic_guard ->
      -- The guard has no precondition.
      case atomic_guard of
        DelayGuard _ -> error "Delay guards in alternatives are unimplemented."
        InputGuard chan _ -> (promise { depth_required = d }, code)
          where (pc, gc) = gen_addr chan
                dc = depth_required pc
                d = if dc > 3 then 5 + dc else max 2 dc
                code ctx =
                  if dc <= 3 then do
                    -- Channel address can be computed without using the stack.
                    cc <- gc ctx
                    return $ Code [cc, Raw [LDC 1, ENBC]]
                  else do
                    -- Channel address requires stack space. Hop over the ALT
                    -- state to compute the channel address, and then hop back to
                    -- execute the enable instruction.
                    let ctx' = allocate ctx "<ALT>" 5
                    cc <- gc ctx'
                    return $ Code [Raw [AJW (-5)], cc, Raw [AJW 5, LDC 1, ENBC]]
        SkipGuard -> (promise { depth_required = 1 }, code)
          where code ctx = return $ Raw [LDC 1, ENBS]
    PrefixedGuard expr atomic_guard ->
      -- The guard has a precondition which must be computed.
      case atomic_guard of
        DelayGuard _ -> error "Delay guards in alternatives are unimplemented."
        InputGuard chan _ -> (promise { depth_required = d }, code)
          where (p, g) = combine (Raw []) conserve_order (gen_addr chan)
                                 (gen_expr expr)
                arg_depth = depth_required p
                d = if arg_depth > 3 then 5 + arg_depth else arg_depth
                code ctx =
                  if arg_depth <= 3 then do
                    -- Channel address can be computed without using the stack.
                    c <- g ctx
                    return $ Code [c, Raw [ENBC]]
                  else do
                    -- Channel address requires stack space. Hop over the ALT
                    -- state to compute the channel address, and then hop back to
                    -- execute the enable instruction.
                    let ctx' = allocate ctx "<ALT>" 5
                    c <- g ctx'
                    return $ Code [Raw [AJW (-5)], c, Raw [AJW 5, ENBC]]
        SkipGuard -> (pe, code)
          where (pe, ge) = gen_expr expr
                code ctx = do
                  ce <- ge ctx
                  return $ Code [ce, Raw [ENBS]]

gen_guard_disable :: Guard -> Process -> DisableGenerator
gen_guard_disable guard proc =
  case guard of
    BasicGuard atomic_guard ->
      -- The guard has no precondition.
      case atomic_guard of
        DelayGuard _ -> error "Delay guards in alternatives are unimplemented."
        InputGuard chan es -> (promise { depth_required = max d d2 }, code)
          where (pc, gc) = gen_addr chan
                code ctx = do
                  service_label <- label "SERVICE"
                  cd <- disable service_label ctx
                  cs <- service service_label ctx
                  return (cd, cs)
                -- PART 1: Generate code for the disable sequence.
                dc = depth_required pc
                d = if dc > 3 then 5 + dc else 3
                disable service_label ctx = do
                  if dc <= 3 then do
                    -- Channel address can be computed without using the stack.
                    cc <- gc ctx
                    return $ Code [cc,
                                   Raw [LDC 1,
                                        LDO service_label (alt_end_label ctx),
                                        DISC]]
                  else do
                    -- Channel address requires stack space. Hop over the ALT
                    -- state to compute the channel address, and then hop back to
                    -- execute the enable instruction.
                    let ctx' = allocate ctx "<ALT>" 5
                    cc <- gc ctx'
                    return $ Code [Raw [AJW (-5)], cc,
                                   Raw [AJW 5, LDC 1,
                                        LDO service_label (alt_end_label ctx'),
                                        DISC]]
                -- PART 2: Generate code for the service routine.
                inputs = map (Input chan) es
                pgs = map gen_proc inputs
                d2 = maximum (map depth_required (map fst pgs ++ [pp]))
                service service_label ctx = do
                  cs <- mapM ($ ctx) (map snd pgs)
                  cp <- gp ctx
                  return $ Code [Label service_label, Code cs, cp,
                                 Raw [J (alt_exit_label ctx)]]
        SkipGuard -> (promise { depth_required = 2 }, code)
          where code ctx = do
                  service_label <- label "SERVICE"
                  cp <- gp ctx
                  return (
                    Raw [LDC 1, LDO service_label (alt_end_label ctx), DISS],
                    Code [Label service_label, cp,
                          Raw [J (alt_exit_label ctx)]])
    PrefixedGuard expr atomic_guard ->
      -- The guard has a precondition which must be computed.
      case atomic_guard of
        DelayGuard _ -> error "Delay guards in alternatives are unimplemented."
        InputGuard chan es -> (promise { depth_required = max d d2 }, code)
          where (p, g) = combine (Raw []) conserve_order (gen_addr chan)
                                 (gen_expr expr)
                code ctx = do
                  service_label <- label "SERVICE"
                  cd <- disable service_label ctx
                  cs <- service service_label ctx
                  return (cd, cs)
                -- PART 1: Generate code for the disable sequence.
                arg_depth = depth_required p
                d = if arg_depth > 3 then 5 + arg_depth else max 3 arg_depth
                disable service_label ctx = do
                  if arg_depth <= 3 then do
                    -- Channel address can be computed without using the stack.
                    c <- g ctx
                    return $ Code [c,
                                   Raw [LDO service_label (alt_end_label ctx),
                                        DISC]]
                  else do
                    -- Channel address requires stack space. Hop over the ALT
                    -- state to compute the channel address, and then hop back to
                    -- execute the enable instruction.
                    let ctx' = allocate ctx "<ALT>" 5
                    c <- g ctx'
                    return $ Code [Raw [AJW (-5)], c,
                                   Raw [LDO service_label (alt_end_label ctx'),
                                        AJW 5, DISC]]
                -- PART 2: Generate code for the service routine.
                inputs = map (Input chan) es
                pgs = map gen_proc inputs
                d2 = maximum (map depth_required (map fst pgs ++ [pp]))
                service service_label ctx = do
                  cs <- mapM ($ ctx) (map snd pgs)
                  cp <- gp ctx
                  return $ Code [Label service_label, Code cs, cp,
                                 Raw [J (alt_exit_label ctx)]]
        SkipGuard -> (pe, code)
          where (pe, ge) = gen_expr expr
                code ctx = do
                  ce <- ge ctx
                  cp <- gp ctx
                  service_label <- label "SERVICE"
                  return (
                    Code [ce, Raw [LDO service_label (alt_end_label ctx),
                                   DISS]],
                    Code [Label service_label, cp,
                          Raw [J (alt_exit_label ctx)]])
  where (pp, gp) = gen_proc proc

gen_nestable_alt_enable :: Nestable Alternative Guard -> CodeGenerator
gen_nestable_alt_enable na =
  case na of
    Nested alt -> gen_alt_enable alt
    Block guard _ -> gen_guard_enable guard

gen_nestable_alt_disable :: Nestable Alternative Guard -> DisableGenerator
gen_nestable_alt_disable na =
  case na of
    Nested alt -> gen_alt_disable alt
    Block guard proc -> gen_guard_disable guard proc

gen_alt_enable :: Alternative -> CodeGenerator
gen_alt_enable (Alternative ra) =
  case ra of
    Basic nas -> (promise { depth_required = d }, code)
      where pgs = map gen_nestable_alt_enable nas
            d = maximum $ map (depth_required . fst) pgs
            code ctx = do
              cs <- sequence $ map (($ ctx) . snd) pgs
              return $ Code cs
    Replicated _ _ ->
      error "Replicated alternatives are unimplemented."

gen_alt_disable :: Alternative -> DisableGenerator
gen_alt_disable (Alternative ra) =
  case ra of
    Basic nas -> (promise { depth_required = d }, code)
      where pgs = map gen_nestable_alt_disable nas
            d = maximum $ map (depth_required . fst) pgs
            code ctx = do
              ccs <- sequence $ map (($ ctx) . snd) pgs
              return (Code (map fst ccs), Code (map snd ccs))
    Replicated _ _ ->
      error "Replicated alternatives are unimplemented."

gen_alt :: Alternative -> CodeGenerator
gen_alt alt = (promise { depth_required = d }, code)
  where (pe, ge) = gen_alt_enable alt
        (pd, gd) = gen_alt_disable alt
        d = maximum $ map depth_required [pe, pd]
        code ctx = do
          ce <- ge ctx
          alt_end_label <- label "ALT_END"
          alt_exit_label <- label "ALT_EXIT"
          let ctx' = ctx { alt_end_label = alt_end_label,
                           alt_exit_label = alt_exit_label }
          (cd, cs) <- gd ctx'
          return $ Code [desc, Raw [ALT], ce, Raw [ALTWT], cd, Raw [ALTEND],
                         Label alt_end_label, cs, Label alt_exit_label]
        desc = comment $ prettyPrint alt

-- Generate code for a parallel block.
gen_par :: Replicable Process -> CodeGenerator
gen_par rp =
  case rp of
    Basic ps -> (promise { depth_required = d }, code)
      where pgs = sortBy (comparing (depth_required . fst)) $ map gen_proc ps
            -- Each sub-process should be given (at least) an extra 5 words of
            -- space which can be used for storing information about the process
            -- when it is descheduled. This list is a list of workspace offsets
            -- for each process.
            ws = scanl (+) 0 $ map ((+5) . depth_required . fst) pgs
            -- Re-zip the generators with the weights rather than the promises.
            wgs = zip (init ws) (map snd pgs)
            -- The last weight is the sum of all the space required, which is
            -- exactly how much space the parallel block will require, minus the
            -- endp block.
            d = 2 + last ws
            code ctx = do
              let ctx' = ctx { stack_depth = stack_depth ctx + 2 }
              lcs <- mapM (gen_subproc ctx') wgs
              let (ls, subproc_code) = unzip lcs
              -- The setup phase spawns new processes to handle all except the
              -- first subprocess. The first subprocess is then executed by the
              -- existing process.
              setup_code <- mapM gen_setup (tail $ zip (init ws) ls)
              end <- label "END_PAR"
              return $ Code [desc,
                             -- Set up the synchronisation monitor.
                             Raw [AJW (-2), LDC (toInteger $ length ps), STL 2,
                                  LDA end, STL 1],
                             Code setup_code, Code subproc_code, Label end,
                             -- Note that the AJW increases the Wptr by 1
                             -- instead of 2. This is intentional: the new
                             -- process spawned by the ENDP starts with a Wptr
                             -- which is 1 larger, so there is 1 fewer spaces to
                             -- deallocate.
                             Raw [AJW 1]]
    Replicated (Range i a b) p -> (promise { depth_required = d }, code)
      where (pa, ga) = gen_expr a
            (pp, gp) = gen_proc p
            -- 2 for the static link and the iteration variable, 5 for
            -- descheduling, and whatever else is required.
            s = 7 + depth_required pp
            n = case b of
                  Value (Integer x) -> x
                    -- Replicated PAR requires a compile-time constant size. If
                    -- this was not ensured by the semantic analysis, something
                    -- has gone wrong.
                  _ -> error "This should never happen."
            d = max (depth_required pa) (4 + n * s)
            code ctx = do
              let ctx1 = allocate ctx i 4
              ca <- ga ctx1
              let ctx2 = allocate (new_static_level ctx1) i 1
              cp <- gp ctx2
              loop <- label "REP_PAR"
              proc <- label "PROC"
              end <- label "END_PAR"
              return $ Code [desc,
                             -- Set up the synchronisation monitor.
                             Raw [AJW (-4), LDC (1 + n), STL 4, LDA end, STL 3],
                             -- Set up the replicator.
                             ca, Raw [STL 1, LDC n, STL 2],
                             -- Begin spawning processes.
                             Label loop,
                             Raw [
                               -- Calculate the new workspace address.
                               LDL 2, ADC (-1), LDC (-s), MUL, LDLP (-7), WSUB,
                               -- Use one copy to initialize the static link.
                               DUP, LDLP (stack_depth ctx1), REV, STNL 2,
                               -- Use another copy to initialize i.
                               DUP, LDL 1, REV, STNL 1,
                               -- Spawn the thread.
                               STARTP proc,
                               -- Continue the loop.
                               LDLP 1, LEND loop],
                             -- End the control process.
                             Raw [LDLP 3, ENDP],
                             -- Code for the repeated process.
                             Label proc, cp,
                             Raw [LDL 2, LDNLP (3 - stack_depth ctx1), ENDP],
                             Label end,
                             -- Deallocate the one remaining allocated unit. See
                             -- the description for the basic version for more
                             -- information.
                             Raw [AJW 1]]
  where desc = comment $ prettyPrint (Par rp)
        -- Generate the parallel setup.
        gen_setup (w, l) = return $ Raw [LDLP (-w), STARTP l]
        -- Generate the code for each sub-process.
        gen_subproc ctx (woff, gp) = do
          proc <- label "PROC"
          let ctx' = ctx { stack_depth = stack_depth ctx + woff }
          cp <- gp ctx'
          let code = Code [Label proc, cp, Raw [LDLP (woff + 1), ENDP]]
          return (proc, code)

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
    Alt alt -> gen_alt alt
    Assign a b -> combine (Code [desc, Raw [STNL 0]]) conserve_order
                          (gen_expr b) (gen_addr a)
    Call "putc" [e] -> (unop e [PUTC])
    Call "print.dec" [e] -> (unop e [PRINTDEC])
    Define t x size proc -> (p, code)
      where (p, gp) = gen_proc proc
            code ctx = do
              let ctx' = allocate ctx x size
              cp <- gp ctx'
              init_code <- initialize t size
              return $ Code [desc, Raw [AJW (-size)],
                             init_code, cp,
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
    DefineProcedure x p -> gen_proc p  -- TODO: Implement procedures.
    If cond -> gen_cond cond
    Input a b -> combine (Code [desc, Raw [IN 4]]) conserve_order
                         (gen_addr b) (gen_addr a)
    Output a b -> combine (Code [desc, Raw [OUTWORD]]) conserve_order
                          (gen_addr a) (gen_expr b)
    Par p -> gen_par p
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
    _ -> error ("Don't know how to compile: " ++ prettyPrint p)
  where desc = comment (prettyPrint p)

-- Run a code generator and output the generated code.
assemble :: Context -> Process -> Handle -> IO ()
assemble ctx process output = do
  let (pp, gp) = gen_proc process
  let text = run_generator (do
        code <- gp ctx
        let mem_size = stack_depth ctx + depth_required pp
        showCode $ Code [Raw [AJW mem_size], code, Raw [STOPP]])
  hPutStr output text
