module Code where

import AnnotatedAST

type Label = String
type Location = Integer

data Operation =
      DoNothing
    | Unimplemented String   -- Code will go here.
    | Constant Integer       -- push(x);
    | NonLocal Integer       -- A = pop(); push(A + 4 * x);
    | Local Integer          -- push(Wptr + 4 * x);
    | Load                   -- push(Mem[pop()]);
    | LoadByte               -- push((byte) Mem[pop()]);
    | Store                  -- Mem[A] = B;
    | StoreByte              -- (byte) Mem[A] = B;
    | Subscript              -- B = pop(); A = pop(); push(4 * B + A);
    | Reverse                -- B = pop(); A = pop(); push(B); push(A);
    | Adjust Integer         -- Wptr += x;
    | Add                    -- B = pop(); A = pop(); push(A + B);
    | Sub                    -- B = pop(); A = pop(); push(A - B);
    | Mul                    -- B = pop(); A = pop(); push(A * B);
    | Div                    -- B = pop(); A = pop(); push(A / B);
    | Mod                    -- B = pop(); A = pop(); push(A % B);
    | Neg                    -- A = pop(); push(-A);
    | CompareEQ              -- B = pop(); A = pop(); push(A == B);
    | CompareLT              -- B = pop(); A = pop(); push(A < B);
    | CompareGT              -- B = pop(); A = pop(); push(A > B);
    | CompareLE              -- B = pop(); A = pop(); push(A <= B);
    | CompareGE              -- B = pop(); A = pop(); push(A >= B);
    | CompareNE              -- B = pop(); A = pop(); push(A != B);
    | BitwiseNot             -- A = pop(); push(¬A);
    | BitwiseAnd             -- B = pop(); A = pop(); push(A & B);
    | BitwiseOr              -- B = pop(); A = pop(); push(A | B);
    | BitwiseXor             -- B = pop(); A = pop(); push(A ^ B);
    | ShiftLeft              -- B = pop(); A = pop(); push(A << B);
    | ShiftRight             -- B = pop(); A = pop(); push(A >> B);  // 0-filled.
    | Jump Label             -- Iptr = x;
    | ConditionalJump Label  -- A = pop(); if (A) jump x;
    | Call Label             -- Call subroutine x.
    | Return                 -- Return from subroutine.
    | StartProcess Label     -- A = pop(); Start process x with workspace A.
    | EndProcess Label       -- A = pop(); End process with refcount workspace A.
    | StopProcess            -- Stop and dequeue the current process.
    | RunProcess             -- A = pop(); Resume process with descriptor A.
    | Input Integer          -- B = pop(); A = pop(); Input x bytes from channel A to location B.
    | Output Integer         -- B = pop(); A = pop(); Output x bytes from location B to channel A.
      -- Compound instructions.
    | AddConstant Integer
    | LoadLocal Integer
    | LoadNonLocal Integer
    | StoreLocal Integer
    | StoreNonLocal Integer
  deriving (Eq, Show)

data Code = Raw Operation    -- An operation.
          | Label Label      -- A label.
          | Code [Code]      -- A sequence of operations.
  deriving (Eq, Show)

data State = State { next_label_id :: Integer }

data Generator a = G (State -> (a, State))

instance Functor Generator where
  fmap f xm = xm >>= return . f

instance Applicative Generator where
  pure = return
  gf <*> gx = gf >>= (\f -> fmap f gx)

instance Monad Generator where
  return x = G (\state -> (x, state))
  (G xm) >>= f = G (\state -> do
    let (x, state') = xm state
    case f x of
      G xm' -> xm' state')

-- Retrieve a unique label containing the given string as a prefix.
label :: String -> Generator Label
label x = G (\state ->
  let id = next_label_id state
  in (x ++ show id, state { next_label_id = id + 1 }))

-- Display the generated code as an assembler string.
showCode :: Code -> String
showCode (Raw x)   = "  " ++ show x ++ "\n"
showCode (Label x) = x ++ ":\n"
showCode (Code cs) = concat $ map showCode cs

gen_expr :: Expression -> Generator Code
gen_expr Any = return $ Raw (Constant 0)
gen_expr (Value (Integer v)) = return $ Raw (Constant v)
