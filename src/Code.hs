module Code where

import Data.List
import Generator
import Operation

data Code = Raw Operation    -- An operation.
          | Label Label      -- A label.
          | Code [Code]      -- A sequence of operations.
  deriving (Eq, Show)

comment :: String -> Code
comment x = Raw (COMMENT x)

assembler_indent = "  "

-- Display the generated code as an assembler string.
showCode :: Code -> String
showCode (Raw x)   =
  (concat . intersperse "\n" . map (assembler_indent ++)) (def x) ++ "\n"
showCode (Label x) = x ++ ":\n\n"
showCode (Code cs) = concat $ map showCode cs
