import qualified Lexer
import Reader
import Data.Char

break_lines :: [Lexer.RawToken] -> [[Lexer.RawToken]]
break_lines = foldr f [[]]
  where f t (l:ls) =
          if Lexer.token_type t == Lexer.NEWLINE then
            [t]:l:ls
          else
            (t:l):ls

-- Strip all instances of SPACES / COMMENTS.
strip_spaces :: [Lexer.RawToken] -> [Lexer.RawToken]
strip_spaces [] = []
strip_spaces (x:xs) =
  case Lexer.token_type x of
    Lexer.SPACES _ -> strip_spaces xs
    Lexer.COMMENT _ -> strip_spaces xs
    _ -> x : strip_spaces xs

-- Normalisation of a line involves:
-- * Removing any whitespace that isn't indentation.
-- * Removing any comments.
normalize_line :: [Lexer.RawToken] -> [Lexer.RawToken]
normalize_line [] = []
normalize_line (x:xs) =
  case Lexer.token_type x of
    Lexer.SPACES _ -> x : strip_spaces xs
    _ -> strip_spaces (x:xs)

-- Token stream representation after indentation has been handled.
data TokenType = CHAR String            -- <Character literal, processed>
               | DEDENT                 -- <decreased indentation>
               | IDENT String           -- 'x'
               | INDENT                 -- <increased indentation>
               | INTEGER Integer        -- '2'
               | KEYWORD Lexer.Keyword  -- <Any Keyword>
               | NEWLINE                -- '\n'
               | STRING String          -- <String literal, processed>
               | SYMBOL Lexer.Symbol    -- <Any symbol>
  deriving (Eq, Show)

type Token = Lexer.Token TokenType

-- Interpret escape sequences in string constants.
interpret_escapes :: String -> String
interpret_escapes [] = []
interpret_escapes ('*':e:cs) = x : interpret_escapes cs
  where x = case toUpper e of
              'C' -> '\r'
              'N' -> '\n'
              'T' -> '\t'
              'S' -> ' '
              '\'' -> '\''
              '"' -> '"'
              '*' -> '*'
              _ -> '?'
interpret_escapes (c:cs) = c : interpret_escapes cs

-- Converts a raw token to its equivalent in the preprocessed format.
-- This should only be called on tokens that can appear in a normalized line.
-- Any other token is treated as an error.
convert :: Lexer.RawType -> TokenType
convert x =
  case x of
    Lexer.CHAR c    -> CHAR $ interpret_escapes c
    Lexer.COMMENT c -> error "Comment in normalized token stream."
    Lexer.IDENT i   -> IDENT i
    Lexer.INTEGER i -> INTEGER i
    Lexer.KEYWORD k -> KEYWORD k
    Lexer.NEWLINE   -> NEWLINE
    Lexer.SPACES n  -> error "Whitespace in normalized token stream."
    Lexer.STRING s  -> STRING $ interpret_escapes s
    Lexer.SYMBOL s  -> SYMBOL s

-- Given a list of lines of RawTokens, produce a list of lines of Tokens.
process_indent :: [[Lexer.RawToken]] -> [[Token]]
process_indent xs = process_indent' 0 xs
  where process_indent' indent [] = [indent_by start (-indent)]
        process_indent' indent (x:xs) =
          let (indent', y) = process_line indent x
          in y : process_indent' indent' xs

-- Given a single line of RawTokens, and the indentation before it, produce the
-- corresponding line of Tokens and the new indentation level.
process_line :: Int -> [Lexer.RawToken] -> (Int, [Token])
process_line indent [] = (indent, [])
process_line indent (x:xs) =
  case Lexer.token_type x of
    Lexer.SPACES s -> f s xs
    _ -> f 0 (x:xs)
  where l = Lexer.token_location x
        f :: Int -> [Lexer.RawToken] -> (Int, [Token])
        f n ys = (n, indent_by l (n - indent) ++ map (fmap convert) ys)

-- Indent (or dedent) by the given amount.
indent_by :: Location -> Int -> [Token]
indent_by loc 0 = []
indent_by loc 1 = error $ "Bad indentation at " ++ show loc
indent_by loc (-1) = error $ "Bad indentation at " ++ show loc
indent_by loc x =
  if x < 0 then
    Lexer.Token DEDENT loc : indent_by loc (x + 2)
  else
    Lexer.Token INDENT loc : indent_by loc (x - 2)

-- Turn a sequence of RawTokens into Tokens, by parsing the indentation.
parse_indent :: [Lexer.RawToken] -> [Token]
parse_indent = concat . process_indent . map normalize_line . break_lines

-- Display the tokens on a line.
show_line :: [Token] -> String
show_line [] = "\n"
show_line (x:xs) = show x ++ "\n" ++ show_line xs

-- Run the lexer!
main = do
  chars <- getContents
  putStr . show . parse_indent . Lexer.tokens Lexer.read_token $ chars
