module Code where

import Data.List
import Generator
import Operation

data Code = Raw Operation    -- An operation.
          | Label Label      -- A label.
          | Code [Code]      -- A sequence of operations.
  deriving (Eq, Show)

-- mappend flattens the code.
instance Monoid Code where
  mempty = Code []
  mappend x (Code []) = x
  mappend (Code []) y = y
  mappend (Code xs) (Code ys) = Code (xs ++ ys)
  mappend x (Code ys) = Code (x : ys)
  mappend (Code xs) y = Code (xs ++ [y])
  mappend x y = Code [x, y]

comment :: String -> Code
comment x = Raw (COMMENT x)

assembler_indent = "  "

-- Display the generated code as an assembler string.
showCode :: Code -> String
showCode (Raw x)   =
  (concat . intersperse "\n" . map (assembler_indent ++)) (def x) ++ "\n"
showCode (Label x) = x ++ ":\n"
showCode (Code cs) = concat $ map showCode cs
