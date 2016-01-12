module AnnotatedAST where

import qualified AST
import Data.Bits
import Data.List
import qualified Data.ByteString as S

-- Scope of a variable.
data Scope = Global | Local
  deriving (Eq, Show)

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

type Name = (Scope, String)

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

data Process = Alt Alternative
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
                | Name Name
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
           | Array [Integer]
           | ChanArray [Integer]
           | ByteArray String
  deriving Eq

instance Show Value where
  show (Integer v) = show v
  show (Array vs) = show vs
  show (ByteArray cs) = show cs

-- Constant values.
two_pow_32 = 0x100000000 :: Integer
two_pow_31 = 0x10000000  :: Integer
true       = 0xFFFFFFFF  :: Integer
false      = 0x00000000  :: Integer

-- Compile-time computation on values.
value a = a `mod` two_pow_32
val_add a b = (a + b) `mod` two_pow_32
val_and a b = if a == true then b else false
val_bitwise_and a b = (a .&. b) `mod` two_pow_32
val_bitwise_or a b = (a .|. b) `mod` two_pow_32
val_bitwise_xor a b = (xor a b) `mod` two_pow_32
val_compare_eq a b = if a == b then true else false
val_compare_ge a b = if a >= b then true else false
val_compare_gt a b = if a > b then true else false
val_compare_le a b = if a <= b then true else false
val_compare_lt a b = if a < b then true else false
val_compare_ne a b = if a /= b then true else false
val_div a b = (a `div` b) `mod` two_pow_32
val_mod a b = (a `mod` b) `mod` two_pow_32
val_mul a b = (a * b) `mod` two_pow_32
val_neg a = (-a) `mod` two_pow_32
val_not a = (complement a) `mod` two_pow_32
val_or a b = if a == false then b else true
val_shift_left a b = (shift a (fromInteger b)) `mod` two_pow_32
val_shift_right a b = (shift a (-fromInteger b)) `mod` two_pow_32
val_sub a b = (a - b) `mod` two_pow_32
