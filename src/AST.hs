module AST where

import Reader

type Name = String

-- AST Node tagged with a location.
data L a = L a Location
  deriving (Eq, Show)

instance Functor L where
  fmap f (L a loc) = L (f a) loc

location :: L a -> Location
location (L _ loc) = loc

node :: L a -> a
node (L x _) = x

-- Generic process.
data Process = Alt Alternative
             | Assign (L Expression) (L Expression)
             | Call (L Name) [L Expression]
             | Definition [L Definition] (L Process)
             | Delay (L Expression)
             | If Condition
             | Input (L Expression) (L Expression)
             | Output (L Expression) (L Expression)
             | Par (Replicable (L Process))
             | PriorityAlt (Alternative)
             | PriorityPar (Replicable (L Process))
             | Seq (Replicable (L Process))
             | Skip
             | Stop
             | Timer (L Expression)
             | While (L Expression) (L Process)
  deriving (Eq, Show)

-- Syntactic literal value.
data Literal = Bool Bool
             | Char Char
             | Integer Integer
             | String String
             | Table ArrayType [L Expression]
  deriving (Eq, Show)

-- Side-effect free calculation.
data Expression = Add [L Expression]
                | After (L Expression) (L Expression)
                | And [L Expression]
                | Any
                | BitwiseAnd [L Expression]
                | BitwiseOr [L Expression]
                | BitwiseXor [L Expression]
                | CompareEQ (L Expression) (L Expression)
                | CompareGE (L Expression) (L Expression)
                | CompareGT (L Expression) (L Expression)
                | CompareLE (L Expression) (L Expression)
                | CompareLT (L Expression) (L Expression)
                | CompareNE (L Expression) (L Expression)
                | Div (L Expression) (L Expression)
                | Index (L Expression) (ArrayType, (L Expression))
                | Literal Literal
                | Mod (L Expression) (L Expression)
                | Mul [L Expression]
                | Neg (L Expression)
                | Not (L Expression)
                | Or [L Expression]
                | ShiftLeft (L Expression) (L Expression)
                | ShiftRight (L Expression) (L Expression)
                | Slice (L Expression) (ArrayType, L Expression, L Expression)
                | Sub (L Expression) (L Expression)
                | Variable Name
  deriving (Eq, Show)

-- i = a FOR b
data Replicator = Range (L Name) (L Expression) (L Expression)
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
                  | Block b (L Process)
  deriving (Eq, Show)

-- ALT
data Alternative = Alternative (Replicable (Nestable (L Alternative) (L Guard)))
  deriving (Eq, Show)

data Guard = BasicGuard (L AtomicGuard)
           | PrefixedGuard (L Expression) (L AtomicGuard)
  deriving (Eq, Show)

data AtomicGuard = DelayGuard (L Expression)
                 | InputGuard (L Expression) [L Expression]
                 | SkipGuard
  deriving (Eq, Show)

-- IF
data Condition = Condition (Replicable (Nestable (L Condition) (L Expression)))
  deriving (Eq, Show)

data RawType = CHAN
             | CONST
             | VALUE
  deriving (Eq, Show)

-- Name definition/declarations, as well as placement.
data Definition = DefineSingle RawType Name
                | DefineVector RawType Name (L Expression)
                | DefineConstant Name (L Expression)
                | DefineProcedure Name [L Formal] (L Process)
  deriving (Eq, Show)

data Formal = Single RawType Name
            | Vector RawType Name
  deriving (Eq, Show)

-- Types that can appear in array definitions, selections, and slices.
data ArrayType = BYTE | INT
  deriving (Eq, Show)

data Program = System (Replicable (L Program))
