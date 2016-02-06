module AnnotatedAST where

import qualified AST
import Data.Bits
import Data.Char
import Data.List
import Text.Printf
import qualified Data.ByteString as S

-- Allocation information for a variable.
data Allocation = Global Integer
                | Local Integer
                | Unknown
  deriving Eq

-- Reference an allocated location from a given workspace location.
reference :: Allocation -> Integer -> Allocation
reference (Global x) _ = Global x
reference (Local x) y = Local (x - y)
reference Unknown _ = Unknown

instance Show Allocation where
  show (Global address) = "[Global " ++ show (Address address) ++ "]"
  show (Local wloc) = "[Local " ++ show wloc ++ "]"
  show Unknown = "[Unknown]"

-- Size information for an array.
data Size = CompileTime Integer
          | Runtime
  deriving Eq

instance Show Size where
  show (CompileTime s) = show s
  show Runtime = ""

-- Type associated with a given name.
data Type = ANY_TYPE  -- Pseudo-type that casts to any other. Used internally.
          | BYTE
          | BYTE_ARRAY Size
          | CHAN
          | CHAN_ARRAY Size
          | CONST Type Value
          | PROC [Type] Process
          | INT
          | INT_ARRAY Size
  deriving Eq

instance Show Type where
  show BYTE = "BYTE"
  show (BYTE_ARRAY n) = "BYTE[" ++ show n ++ "]"
  show CHAN = "CHAN"
  show (CHAN_ARRAY n) = "CHAN[" ++ show n ++ "]"
  show (CONST t v) = "CONST " ++ show t ++ " " ++ show v
  show (PROC ts _) =
      "PROC(" ++ (concat . intersperse ", " . map show $ ts) ++ ")"
  show INT = "INT"
  show (INT_ARRAY n) = "INT[" ++ show n ++ "]"

-- Convert raw type to type.
raw_type :: AST.RawType -> Type
raw_type AST.CHAN = CHAN
raw_type AST.VALUE = INT
raw_type t = error ("This raw type (" ++ show t ++ ") should not appear in the AST.")

type Name = String

-- i = a FOR b
data Replicator = Range Name Expression Expression
  deriving (Eq, Show)

-- Helper for blocks that can be replicated.
data Replicable a = Basic [a]
                  | Replicated Replicator a
  deriving (Eq, Show)

-- Helper for blocks which have syntax approximately like:
-- A = foo
--       B
-- B = A
--   | bar
--       baz
data Nestable a b = Nested a
                  | Block b Process
  deriving (Eq, Show)

data Process = Allocate Integer Process
             | Alt Alternative
             | Assign Expression Expression
             | Call Name [Expression]
             | Delay Expression
             | If Condition
             | Input Expression Expression
             | Output Expression Expression
             | Par (Replicable Process)
             | PriorityAlt Alternative
             | PriorityPar (Replicable Process)
             | Seq (Replicable Process)
             | Skip
             | Stop
             | Timer Expression
             | While Expression Process
  deriving (Eq, Show)

data Expression = Add [Expression]
                | After Expression Expression
                | And [Expression]
                | Any
                | BitwiseAnd [Expression]
                | BitwiseOr [Expression]
                | BitwiseXor [Expression]
                | CompareEQ Expression Expression
                | CompareGE Expression Expression
                | CompareGT Expression Expression
                | CompareLE Expression Expression
                | CompareLT Expression Expression
                | CompareNE Expression Expression
                | Div Expression Expression
                | Index Expression (AST.ArrayType, Expression)
                | Mod Expression Expression
                | Mul [Expression]
                | Neg Expression
                | Not Expression
                | Or [Expression]
                | ShiftLeft Expression Expression
                | ShiftRight Expression Expression
                | Slice Expression (AST.ArrayType, Expression, Expression)
                | Sub Expression Expression
                | Value Value
                | Name Allocation Name
  deriving (Eq, Show)

data Alternative = Alternative (Replicable (Nestable Alternative Guard))
  deriving (Eq, Show)

data Guard = BasicGuard AtomicGuard
           | PrefixedGuard Expression AtomicGuard
  deriving (Eq, Show)

data AtomicGuard = DelayGuard Expression
                 | InputGuard Expression [Expression]
                 | SkipGuard
  deriving (Eq, Show)

-- IF
data Condition = Condition (Replicable (Nestable Condition Expression))
  deriving (Eq, Show)

-- Compile-time constant value.
data Value = Integer Integer
           | Address Integer
  deriving Eq

instance Show Value where
  show (Integer i) = show i
  show (Address a) = printf "0x%08x" (a + if a < 0 then two_pow_32 else 0)

-- Constant values.
two_pow_32 = 0x100000000 :: Integer
two_pow_31 = 0x10000000  :: Integer
mem_start  = value 0x80000070
true       = value 1
false      = value 0

-- Compile-time computation on values.
-- TODO: Adjust these to agree with the transputer calculations.
value a = if x >= two_pow_31 then x - two_pow_32 else x
  where x = a `mod` two_pow_32
val_add a b = value (a + b)
val_and a b = if a == true then b else false
val_bitwise_and a b = value (a .&. b)
val_bitwise_or a b = value (a .|. b)
val_bitwise_xor a b = value (xor a b)
val_compare_eq a b = if a == b then true else false
val_compare_ge a b = if a >= b then true else false
val_compare_gt a b = if a > b then true else false
val_compare_le a b = if a <= b then true else false
val_compare_lt a b = if a < b then true else false
val_compare_ne a b = if a /= b then true else false
val_div a b = value (a `div` b)
val_mod a b = value (a `mod` b)
val_mul a b = value (a * b)
val_neg a = value (-a)
val_not a = value (complement a)
val_or a b = if a == false then b else true
val_shift_left a b = value (shift a (fromInteger b))
val_shift_right a b = value (shift a (-fromInteger b))
val_sub a b = value (a - b)
