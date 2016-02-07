module Operation where

import Data.List
import Generator
import Prelude hiding (EQ, GT)

data Operation =
      NOP
    | COMMENT String
    | ADC Integer    -- A += x;
    | ADD            -- B = pop(); A = pop(); push(A + B);
    | AJW Integer    -- Wptr += x;
    | AND            -- B = pop(); A = pop(); push(A & B);
    | CALL Label     -- Call subroutine x.
    | CJ Label       -- A = pop(); if (A) jump x;
    | DIFF           -- A -= B;
    | DIV            -- B = pop(); A = pop(); push(A / B);
    | ENDP           -- A = pop(); End process with refcount workspace A.
    | EQ             -- B = pop(); A = pop(); push(A == B);
    | EQC Integer    -- A = pop(); push(A == x);
    | GT             -- B = pop(); A = pop(); push(A > B);
    | IN Integer     -- B = pop(); A = pop(); Input x bytes from channel A to location B.
    | J Label        -- Iptr = x;
    | LB             -- push((byte) Mem[pop()]);
    | LDC Integer    -- push(x);
    | LDL Integer    -- push(Mem[Wptr + 4 * x]);
    | LDLP Integer   -- push(Wptr + 4 * x);
    | LDNL Integer   -- A = pop(); push(Mem[A + 4 * x]);
    | LDNLP Integer  -- A = pop(); push(A + 4 * x);
    | MUL            -- B = pop(); A = pop(); push(A * B);
    | NOT            -- A = pop(); push(¬A);
    | OR             -- B = pop(); A = pop(); push(A | B);
    | OUT Integer    -- B = pop(); A = pop(); Output x bytes from location B to channel A.
    | REM            -- B = pop(); A = pop(); push(A % B);
    | RET            -- Return from subroutine.
    | REV            -- B = pop(); A = pop(); push(B); push(A);
    | RUNP           -- A = pop(); Resume process with descriptor A.
    | SB             -- B = pop(); A = pop(); (byte) Mem[A] = B;
    | SHL            -- B = pop(); A = pop(); push(A << B);
    | SHR            -- B = pop(); A = pop(); push(A >> B);  // 0-filled.
    | STARTP         -- Start process x with workspace y.
    | STL Integer    -- Mem[Wptr + 4 * x] = A;
    | STNL Integer   -- Mem[A + 4 * x] = B;
    | STOPP          -- Stop and dequeue the current process.
    | SUB            -- B = pop(); A = pop(); push(A - B);
    | WSUB           -- B = pop(); A = pop(); push(4 * B + A);
    | XOR            -- B = pop(); A = pop(); push(A ^ B);
  deriving (Eq, Show)

-- Sequence of assembler instructions defining the given operation.
def :: Operation -> [String]
def NOP               = ["# NOP"]
def (COMMENT x) = ["# " ++ x]
def (ADC x)    = ["adc " ++ show x]
def ADD        = ["add"]
def (AJW x)    = ["ajw " ++ show x]
def AND        = ["and"]
def (CALL x)   = ["call " ++ x]
def (CJ x)     = ["cj " ++ x]
def DIFF       = ["diff"]
def DIV        = ["div"]
def ENDP       = ["endp"]
def EQ         = ["diff", "eqc 0"]
def (EQC x)    = ["eqc " ++ show x]
def GT         = ["gt"]
def (IN x)     = ["ldc " ++ show x, "in"]
def (J x)      = ["j " ++ x]
def LB         = ["lb"]
def (LDC x)    = ["ldc " ++ show x]
def (LDL x)    = ["ldl " ++ show x]
def (LDLP x)   = ["ldlp " ++ show x]
def (LDNL x)   = ["ldnl " ++ show x]
def (LDNLP x)  = ["ldnlp " ++ show x]
def MUL        = ["mul"]
def NOT        = ["eqc 0"]
def OR         = ["or"]
def (OUT x)    = ["ldc " ++ show x, "out"]
def REM        = ["rem"]
def RET        = ["ret"]
def REV        = ["rev"]
def RUNP       = ["runp"]
def SB         = ["sb"]
def SHL        = ["shl"]
def SHR        = ["shr"]
def STARTP     = ["startp"]
def (STL x)    = ["stl " ++ show x]
def (STNL x)   = ["stnl " ++ show x]
def STOPP      = ["stopp"]
def SUB        = ["sub"]
def WSUB       = ["wsub"]
def XOR        = ["xor"]
