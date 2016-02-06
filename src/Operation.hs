module Operation where

type Label = String

data Operation =
      NOP
    | UNIMPLEMENTED String   -- Code will go here.
    | CONSTANT Integer       -- push(x);
    | NON_LOCAL Integer      -- A = pop(); push(A + 4 * x);
    | LOCAL Integer          -- push(Wptr + 4 * x);
    | LOAD                   -- push(Mem[pop()]);
    | LOAD_BYTE              -- push((byte) Mem[pop()]);
    | STORE                  -- B = pop(); A = pop(); Mem[B] = A;
    | STORE_BYTE             -- B = pop(); A = pop(); (byte) Mem[A] = B;
    | SUBSCRIPT              -- B = pop(); A = pop(); push(4 * B + A);
    | REVERSE                -- B = pop(); A = pop(); push(B); push(A);
    | ADJUST Integer         -- Wptr += x;
    | AFTER                  -- B = pop(); A = pop(); push(A after B);
    | ADD                    -- B = pop(); A = pop(); push(A + B);
    | SUB                    -- B = pop(); A = pop(); push(A - B);
    | MUL                    -- B = pop(); A = pop(); push(A * B);
    | DIV                    -- B = pop(); A = pop(); push(A / B);
    | MOD                    -- B = pop(); A = pop(); push(A % B);
    | NEG                    -- A = pop(); push(-A);
    | COMPARE_EQ             -- B = pop(); A = pop(); push(A == B);
    | COMPARE_GT             -- B = pop(); A = pop(); push(A > B);
    | LOGICAL_NOT            -- A = pop(); push(A == 0 ? 1 : 0);
    | BITWISE_NOT            -- A = pop(); push(¬A);
    | BITWISE_AND            -- B = pop(); A = pop(); push(A & B);
    | BITWISE_OR             -- B = pop(); A = pop(); push(A | B);
    | BITWISE_XOR            -- B = pop(); A = pop(); push(A ^ B);
    | SHIFT_LEFT             -- B = pop(); A = pop(); push(A << B);
    | SHIFT_RIGHT            -- B = pop(); A = pop(); push(A >> B);  // 0-filled.
    | JUMP Label             -- Iptr = x;
    | COND_JUMP Label        -- A = pop(); if (A) jump x;
    | CALL Label             -- Call subroutine x.
    | RETURN                 -- Return from subroutine.
    | START_PROC Label       -- A = pop(); Start process x with workspace A.
    | END_PROC               -- A = pop(); End process with refcount workspace A.
    | STOP_PROC              -- Stop and dequeue the current process.
    | RUN_PROC               -- A = pop(); Resume process with descriptor A.
    | INPUT Integer          -- B = pop(); A = pop(); Input x bytes from channel A to location B.
    | OUTPUT Integer         -- B = pop(); A = pop(); Output x bytes from location B to channel A.
      -- Compound instructions.
    | ADD_CONSTANT Integer
    | LOAD_LOCAL Integer
    | LOAD_NON_LOCAL Integer
    | STORE_LOCAL Integer
    | STORE_NON_LOCAL Integer
  deriving (Eq, Show)

-- Convert an operation to a string of assembler.
showOp :: Operation -> String
showOp op = unlines (("# " ++ show op) : def op)

-- Standard wrapper for operations which loads/saves the inputs/outputs.
op :: Integer -> Integer -> [String] -> [String]
op a b xs =
  if a < b then
    -- More outputs than inputs.
    ["ajw " ++ show (a - b)] ++
    map (("ldl " ++) . show) [b, b - 1 .. b - a + 1] ++
    xs ++
    map (("stl " ++) . show) [b, b - 1 .. 1]
  else
    -- More inputs than outputs.
    map (("ldl " ++) . show) [a, a - 1 .. 1] ++
    xs ++
    map (("stl " ++) . show) [a, a - 1 .. a - b + 1] ++
    ["ajw " ++ show (a - b)]

-- Sequence of assembler instructions defining the given operation.
def :: Operation -> [String]
def NOP               = op 1 1 ["adc 0"]
def (UNIMPLEMENTED x) = op 0 0 ["# UNIMPLEMENTED: " ++ x]
def (CONSTANT x)      = op 0 1 ["ldc " ++ show x]
def (NON_LOCAL x)     = op 1 1 ["ldnlp " ++ show x]
def (LOCAL x)         = op 0 1 ["ldlp " ++ show x]
def LOAD              = op 1 1 ["ldnl 0"]
def LOAD_BYTE         = op 1 1 ["lb"]
def STORE             = op 2 0 ["stnl 0"]
def STORE_BYTE        = op 2 0 ["sb"]
def SUBSCRIPT         = op 2 1 ["wsub"]
def REVERSE           = op 2 2 ["rev"]
def (ADJUST x)        = op 0 0 ["ajw " ++ show x]
def AFTER             = op 2 1 ["diff", "gt"]
def ADD               = op 2 1 ["add"]
def SUB               = op 2 1 ["sub"]
def MUL               = op 2 1 ["mul"]
def DIV               = op 2 1 ["div"]
def MOD               = op 2 1 ["rem"]
def NEG               = op 1 1 ["not", "adc 1"]
def COMPARE_EQ        = op 2 1 ["diff", "eqc 0"]
def COMPARE_GT        = op 2 1 ["gt"]
def LOGICAL_NOT       = op 1 1 ["eqc 0"]
def BITWISE_NOT       = op 1 1 ["not"]
def BITWISE_AND       = op 2 1 ["and"]
def BITWISE_OR        = op 2 1 ["or"]
def BITWISE_XOR       = op 2 1 ["xor"]
def SHIFT_LEFT        = op 2 1 ["shl"]
def SHIFT_RIGHT       = op 2 1 ["shr"]
def (JUMP x)          = op 0 0 ["j " ++ x]
def (COND_JUMP x)     = op 1 0 ["cj " ++ x]
def (CALL x)          = op 1 0 ["call " ++ x]
def RETURN            = op 0 0 ["ret"]
def (START_PROC x)    = ["ldc " ++ x] ++ op 1 0 ["startp"]
def END_PROC          = op 1 0 ["endp"]
def STOP_PROC         = op 0 0 ["stopp"]
def RUN_PROC          = op 1 0 ["runp"]
def (INPUT x)         = op 2 0 ["ldc " ++ show x, "in"]
def (OUTPUT x)        = op 2 0 ["ldc " ++ show x, "out"]
