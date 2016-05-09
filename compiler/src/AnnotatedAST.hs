module AnnotatedAST where

import qualified AST
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Text.Printf
import qualified Data.ByteString as S

-- Pretty-printing.
class Pretty a where
  prettyPrint :: a -> String

-- Allocation information for a variable.
data Allocation = Global Int32       -- Global address.
                | Local Int32 Int32  -- Static link, local offset.
                | Constant Int32     -- Constant integer value.
  deriving Eq

instance Show Allocation where
  show (Global address) = "[Global " ++ show (Address address) ++ "]"
  show (Local sl off) = "[Local " ++ show off ++ ", SL = " ++ show sl ++ "]"
  show (Constant x) = "[Constant " ++ show x ++ "]"

-- Type associated with a given name.
data Type = ANY_TYPE  -- Pseudo-type that casts to any other. Used internally.
          | BYTE
          | BYTE_ARRAY Int32
          | CHAN
          | CHAN_ARRAY Int32
          | CONST Type Value
          | PROC [Type] Process
          | INT
          | INT_ARRAY Int32
          | INT_REF
          | INT_ARRAY_REF
          | CHAN_ARRAY_REF
          | BYTE_ARRAY_REF
  deriving Eq

instance Show Type where
  show ANY_TYPE = "ANY"
  show BYTE = "BYTE"
  show (BYTE_ARRAY n) = "BYTE[" ++ show n ++ "]"
  show CHAN = "CHAN"
  show (CHAN_ARRAY n) = "CHAN[" ++ show n ++ "]"
  show (CONST t v) = "CONST " ++ show t ++ " " ++ show v
  show (PROC ts _) =
      "PROC(" ++ (concat . intersperse ", " . map show $ ts) ++ ")"
  show INT = "INT"
  show (INT_ARRAY n) = "INT[" ++ show n ++ "]"
  show INT_REF = "INT REF"
  show INT_ARRAY_REF = "INT[] REF"
  show CHAN_ARRAY_REF = "CHAN[] REF"
  show BYTE_ARRAY_REF = "BYTE[] REF"

-- Returns the amount of space (in words) required in the workspace for a
-- particular data type.
space_needed :: Type -> Int32
space_needed ANY_TYPE = 0                      -- Use some temporary space.
space_needed BYTE = 1                          -- Use a word for storing bytes.
space_needed (BYTE_ARRAY x) = (x + 3) `div` 4  -- Round up to the next word.
space_needed CHAN = 1                          -- Channels are one word in size.
space_needed (CHAN_ARRAY x) = x                -- Array of channels.
space_needed (CONST _ _) = 0                   -- Compile-time substituted.
space_needed (PROC _ _) = 0                    -- Stored elsewhere.
space_needed INT = 1                           -- Integers are one word each.
space_needed (INT_ARRAY x) = x                 -- Array of integers.
space_needed INT_REF = 1                       -- address
space_needed INT_ARRAY_REF = 2                 -- (address, size).
space_needed CHAN_ARRAY_REF = 2                -- (address, size).
space_needed BYTE_ARRAY_REF = 2                -- (address, size).

-- Convert raw type to type.
raw_type :: AST.RawType -> Type
raw_type AST.CHAN = CHAN
raw_type AST.VALUE = INT
raw_type t =
  error ("This raw type (" ++ show t ++ ") should not appear in the AST.")

-- Convert raw type to array of type.
raw_array_type :: AST.RawType -> Int32 -> Type
raw_array_type AST.CHAN size = CHAN_ARRAY size
raw_array_type AST.VALUE size = INT_ARRAY size
raw_array_type t _ =
  error ("This raw type (" ++ show t ++ ") should not appear in the AST.")

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

data Process = Alt Alternative
             | Assign Expression Expression
             | Call Name [Expression]
             | Define AST.RawType Name Int32 Process
             | DefineConstant Name Value Process
             | DefineProcedure Name Process
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

prettyRep :: String -> Replicable a -> String
prettyRep block_type (Basic as) = block_type ++ " ..."
prettyRep block_type (Replicated (Range x a b) p) =
  block_type ++ " " ++ x ++ " = [" ++ prettyPrint a ++ " FOR " ++
  prettyPrint b ++ "] ..."

instance Pretty Process where
  prettyPrint (Alt (Alternative ra)) = prettyRep "ALT" ra
  prettyPrint (Assign a b) =
    prettyPrint a ++ " := " ++ prettyPrint b
  prettyPrint (Call x es) =
    x ++ "(" ++ (concat . intersperse ", " . map prettyPrint $ es) ++ ")"
  prettyPrint (Define t x s p) =
    spec ++ " " ++ x ++ (if s == 1 then "" else "[" ++ show s ++ "]")
    where spec = case t of
                   AST.VALUE -> "VAR"
                   AST.CHAN -> "CHAN"
  prettyPrint (DefineConstant x v p) =
    "DEF " ++ x ++ " = " ++ prettyPrint (Value v)
  prettyPrint (DefineProcedure name p) =
    "PROC " ++ name ++ "(...) = ..."
  prettyPrint (Delay e) =
    "TIME ? AFTER " ++ prettyPrint e
  prettyPrint (If (Condition rc)) = prettyRep "IF" rc
  prettyPrint (Input a b) =
    prettyPrint a ++ " ? " ++ prettyPrint b
  prettyPrint (Output a b) =
    prettyPrint a ++ " ! " ++ prettyPrint b
  prettyPrint (Par rp) = prettyRep "PAR" rp
  prettyPrint (PriorityAlt (Alternative ra)) = prettyRep "PRI ALT" ra
  prettyPrint (PriorityPar rp) = prettyRep "PRI PAR" rp
  prettyPrint (Seq rp) = prettyRep "SEQ" rp
  prettyPrint Skip = "SKIP"
  prettyPrint Stop = "STOP"
  prettyPrint (Timer e) = "TIME ? " ++ prettyPrint e
  prettyPrint (While e p) = "WHILE " ++ prettyPrint e

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

pp_binop :: String -> Expression -> Expression -> String
pp_binop x a b =
  "(" ++ prettyPrint a ++ " " ++ x ++ " " ++ prettyPrint b ++ ")"

pp_assoc :: String -> [Expression] -> String
pp_assoc x as =
  "(" ++ concat (intersperse (" " ++ x ++ " ") (map prettyPrint as)) ++ ")"

instance Pretty Expression where
  prettyPrint (Add es) = pp_assoc "+" es
  prettyPrint (After a b) = pp_binop "AFTER" a b
  prettyPrint (And es) = pp_assoc "AND" es
  prettyPrint Any = "ANY"
  prettyPrint (BitwiseAnd es) = pp_assoc "/\\" es
  prettyPrint (BitwiseOr es) = pp_assoc "\\/" es
  prettyPrint (BitwiseXor es) = pp_assoc "><" es
  prettyPrint (CompareEQ a b) = pp_binop "=" a b
  prettyPrint (CompareGE a b) = pp_binop ">=" a b
  prettyPrint (CompareGT a b) = pp_binop ">" a b
  prettyPrint (CompareLE a b) = pp_binop "<=" a b
  prettyPrint (CompareLT a b) = pp_binop "<" a b
  prettyPrint (CompareNE a b) = pp_binop "<>" a b
  prettyPrint (Div a b) = pp_binop "/" a b
  prettyPrint (Index a (t, b)) =
    prettyPrint a ++ "[" ++
    (if t == AST.BYTE then "BYTE " else "") ++
    prettyPrint b ++ "]"
  prettyPrint (Mod a b) = pp_binop "\\" a b
  prettyPrint (Mul es) = pp_assoc "*" es
  prettyPrint (Neg e) = "(-" ++ prettyPrint e ++ ")"
  prettyPrint (Not e) = "(NOT " ++ prettyPrint e ++ ")"
  prettyPrint (Or es) = pp_assoc "OR" es
  prettyPrint (ShiftLeft a b) = pp_binop "<<" a b
  prettyPrint (ShiftRight a b) = pp_binop ">>" a b
  prettyPrint (Slice e (t, a, b)) =
    prettyPrint e ++ "[" ++
    (if t == AST.BYTE then "BYTE " else "") ++
    prettyPrint a ++ " FOR " ++ prettyPrint b ++ "]"
  prettyPrint (Sub a b) = pp_binop "-" a b
  prettyPrint (Value x) = show x
  prettyPrint (Name x) = x

data Alternative = Alternative (Replicable (Nestable Alternative Guard))
  deriving (Eq, Show)

instance Pretty Alternative where
  prettyPrint (Alternative ra) = prettyRep "ALT" ra

data Guard = BasicGuard AtomicGuard
           | PrefixedGuard Expression AtomicGuard
  deriving (Eq, Show)

instance Pretty Guard where
  prettyPrint (BasicGuard a) = prettyPrint a
  prettyPrint (PrefixedGuard cond a) =
    prettyPrint cond ++ " & " ++ prettyPrint a

data AtomicGuard = DelayGuard Expression
                 | InputGuard Expression [Expression]
                 | SkipGuard
  deriving (Eq, Show)

instance Pretty AtomicGuard where
  prettyPrint (DelayGuard expr) = "AFTER " ++ prettyPrint expr
  prettyPrint (InputGuard c (e:[])) = prettyPrint c ++ " ? " ++ prettyPrint e
  prettyPrint (InputGuard c (e:es)) =
    prettyPrint c ++ " ? " ++ prettyPrint e ++ "; ..."
  prettyPrint SkipGuard = "SKIP"

-- IF
data Condition = Condition (Replicable (Nestable Condition Expression))
  deriving (Eq, Show)

instance Pretty Condition where
  prettyPrint (Condition rc) = prettyRep "IF" rc

-- Compile-time constant value.
data Value = Integer Int32
           | Address Int32
  deriving Eq

instance Show Value where
  show (Integer i) = show i
  show (Address a) = printf "0x%08x" a

-- Constant values.
two_pow_31 = value 0x80000000
true       = value 1
false      = value 0

-- Compile-time computation on values.
-- TODO: Adjust these to agree with the transputer calculations.
value a = (fromIntegral a) :: Int32
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
val_shift_left a b = value (shift a (fromIntegral b))
val_shift_right a b = value (shift a (-fromIntegral b))
val_sub a b = value (a - b)
