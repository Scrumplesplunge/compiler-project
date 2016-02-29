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
    | DUP            -- push(A);
    | ENDP           -- A = pop(); End process with refcount workspace A.
    | EQ             -- B = pop(); A = pop(); push(A == B);
    | EQC Integer    -- A = pop(); push(A == x);
    | GT             -- B = pop(); A = pop(); push(A > B);
    | IN Integer     -- B = pop(); A = pop(); Input x bytes from channel A to location B.
    | J Label        -- Iptr = x;
    | LB             -- push((byte) Mem[pop()]);
    | LDA Label      -- Load the (non-relative) address of a label.
    | LDC Integer    -- push(x);
    | LDL Integer    -- push(Mem[Wptr + 4 * x]);
    | LDLP Integer   -- push(Wptr + 4 * x);
    | LDNL Integer   -- A = pop(); push(Mem[A + 4 * x]);
    | LDNLP Integer  -- A = pop(); push(A + 4 * x);
    | LEND Label     -- Loop end.
    | MINT           -- Minimum integer.
    | MUL            -- B = pop(); A = pop(); push(A * B);
    | NOT            -- A = pop(); push(¬A);
    | OR             -- B = pop(); A = pop(); push(A | B);
    | OUT Integer    -- B = pop(); A = pop(); Output x bytes from location B to channel A.
    | OUTWORD        -- Output word B to channel A.
    | REM            -- B = pop(); A = pop(); push(A % B);
    | RET            -- Return from subroutine.
    | REV            -- B = pop(); A = pop(); push(B); push(A);
    | RUNP           -- A = pop(); Resume process with descriptor A.
    | SB             -- B = pop(); A = pop(); (byte) Mem[A] = B;
    | SHL            -- B = pop(); A = pop(); push(A << B);
    | SHR            -- B = pop(); A = pop(); push(A >> B);  // 0-filled.
    | STARTP Label   -- Start process x with workspace y.
    | STL Integer    -- Mem[Wptr + 4 * x] = A;
    | STNL Integer   -- Mem[A + 4 * x] = B;
    | STOPP          -- Stop and dequeue the current process.
    | SUB            -- B = pop(); A = pop(); push(A - B);
    | WSUB           -- B = pop(); A = pop(); push(4 * B + A);
    | XOR            -- B = pop(); A = pop(); push(A ^ B);
    -- Meta operations.
    | PUTC           -- Put a character on the screen.
    | PRINTDEC       -- Print a decimal integer.
  deriving (Eq, Show)

-- Load a label relative to the end of an instruction sequence.
relative :: String -> [String] -> Generator [String]
relative target_label ops = do
  l <- label "HERE"
  return (["ldc " ++ target_label ++ " - " ++ l] ++
          ops ++ [l ++ ":  # Sneaky label."])

-- Sequence of assembler instructions defining the given operation.
def :: Operation -> Generator [String]
def NOP          = return ["# NOP"]
def (COMMENT x)  = return ["# " ++ x]
def (ADC x)      = return ["adc " ++ show x]
def ADD          = return ["add"]
def (AJW x)      = return ["ajw " ++ show x]
def AND          = return ["and"]
def (CALL x)     = return ["call " ++ x]
def (CJ x)       = return ["cj " ++ x]
def DIFF         = return ["diff"]
def DIV          = return ["div"]
def DUP          = return ["dup"]
def ENDP         = return ["endp"]
def EQ           = return ["diff", "eqc 0"]
def (EQC x)      = return ["eqc " ++ show x]
def GT           = return ["gt"]
def (IN x)       = return ["ldc " ++ show x, "in"]
def (J x)        = return ["j " ++ x]
def LB           = return ["lb"]
def (LDA x)      = relative x ["ldpi"]
def (LDC x)      = return ["ldc " ++ show x]
def (LDL x)      = return ["ldl " ++ show x]
def (LDLP x)     = return ["ldlp " ++ show x]
def (LDNL x)     = return ["ldnl " ++ show x]
def (LDNLP x)    = return ["ldnlp " ++ show x]
def (LEND x)     = relative x ["lend"]
def MINT         = return ["mint"]
def MUL          = return ["mul"]
def NOT          = return ["eqc 0"]
def OR           = return ["or"]
def (OUT x)      = return ["ldc " ++ show x, "out"]
def OUTWORD      = return ["outword"]
def REM          = return ["rem"]
def RET          = return ["ret"]
def REV          = return ["rev"]
def RUNP         = return ["runp"]
def SB           = return ["sb"]
def SHL          = return ["shl"]
def SHR          = return ["shr"]
def (STARTP i) = relative i ["rev", "startp"]
def (STL x)      = return ["stl " ++ show x]
def (STNL x)     = return ["stnl " ++ show x]
def STOPP        = return ["stopp"]
def SUB          = return ["sub"]
def WSUB         = return ["wsub"]
def XOR          = return ["xor"]

def PUTC         = return ["putc"]
def PRINTDEC     = return ["printdec"]
