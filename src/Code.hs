module Code where

import Data.List
import Generator
import Operation

data Code = Raw [Operation]  -- An operation.
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
comment x = Raw [COMMENT x]

assembler_indent = "  "

-- Display the generated code as an assembler string.
showCode :: Code -> Generator String
showCode code =
  case code of
    (Raw xs) -> do
      xs' <- mapM def xs
      return $ indent (concat xs') ++ "\n"
      where indent = concat . intersperse "\n" . map (assembler_indent ++)
    (Label x) -> return $ x ++ ":\n"
    (Code cs) -> do
      cs' <- mapM showCode cs
      return $ concat cs'
