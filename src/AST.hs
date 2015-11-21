module AST where

-- Literal values (except booleans)
data Literal = Char Char
             | Integer Integer
             | String String
  deriving (Eq, Show)

-- Boolean condition.
data Condition = And Condition Condition
               | BitwiseAnd Expr Expr
               | BitwiseOr Expr Expr
               | BitwiseXor Expr Expr
               | CompareEQ Expr Expr
               | CompareGE Expr Expr
               | CompareGT Expr Expr
               | CompareLE Expr Expr
               | CompareLT Expr Expr
               | CompareNE Expr Expr
               | Not Condition
               | Or Condition Condition
               | Xor Condition Condition
               | Invariably Bool          -- Literal booleans.
  deriving (Eq, Show)

-- Pure computation (that is, no side-effects!)
data Expr = Add Expr Expr
          | Condition Condition
          | Div Expr Expr
          | Literal Literal
          | Mul Expr Expr
          | Neg Expr
          | RArray Expr
          | RVariable String
          | Sub Expr Expr
  deriving (Eq, Show)

-- Things that can be assigned to variables.
data RValue = Expr Expr
  deriving (Eq, Show)

-- Things that can be assigned to.
data LValue = LArray Expr
            | LVariable String
  deriving (Eq, Show)

-- For loop ranges.
type Range = Maybe (String, Expr, Expr)

-- Definitions.
data Definition = DefineChannel String
                | DefineConstant String Expr
                | DefineProcedure String [(VarType, String)] Process
                | DefineVariable String
  deriving (Eq, Show)

-- Algorithms.
data Process = Alternation Range [(Condition, String, Process)]
             | Assignment LValue RValue
             | Call String [RValue]
             | Definition Definition Process
             | If Range [(Condition, Process)]
             | Input RValue LValue
             | Output LValue RValue
             | Parallel Range [Process]
             | Sequence Range [Process]
             | Skip
             | Stop
             | While Condition Process
  deriving (Eq, Show)

data Program = Program Process
  deriving (Eq, Show)

data VarType = ARRAY VarType Integer
             | CHAN
             | PROC [VarType]
             | VALUE
             | VAR
             | VARARRAY VarType
  deriving (Eq, Show)
