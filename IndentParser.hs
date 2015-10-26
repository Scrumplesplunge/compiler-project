module IndentParser where

import qualified Lexer
import Reader
import Data.Char

-- Takes a line and returns how much it is indented by.
indent :: [Lexer.RawToken] -> Int
indent [] = 0
indent (t:ts) =
  case Lexer.token_type t of
    Lexer.SPACES s -> s
    _ -> 0

-- Returns true if the line ends with a continuation token.
continues :: [Lexer.RawToken] -> Bool
continues [] = False
continues [t] = False
continues [s, t] = Lexer.is_continuation_token s
continues (r:s:t:ts) = continues (s:t:ts)

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

-- Remove leading whitespace from continuation lines.
process_indent :: [[Lexer.RawToken]] -> [[Token]]
process_indent ls = process_indent' ls 0 False

-- Normalize a continuation line. That is, if the line ends with a continuation
-- token, remove the NEWLINE.
f :: Bool -> Int -> [[Lexer.RawToken]] -> [Token] -> [[Token]]
f c new_indent ls l =
  (if c then init l else l) : process_indent' ls new_indent c

-- Internals of process indent: Takes lines, indent, and whether or not to treat
-- the first line as a continuation line, and produces lines of tokens with
-- correct indentation.
process_indent' :: [[Lexer.RawToken]] -> Int -> Bool -> [[Token]]
process_indent' [] _ _ = []
process_indent' (l:ls) previous_indent True =
  if indent l < previous_indent then
    let (Lexer.Token t loc) = head l in
      error $ "Bad indentation at " ++ show loc
  else
    f (continues l) previous_indent ls $ map (fmap convert) $ strip_spaces l
    -- map (fmap convert) (strip_spaces l) : process_indent' ls prev (continues l)
process_indent' (l:ls) previous_indent False =
  let (new_indent, ts) = process_line previous_indent l in
    f (continues l) new_indent ls ts
    -- ts : process_indent' ls new_indent (continues l)

-- Given a single line of RawTokens, and the indentation before it, produce the
-- corresponding line of Tokens as well as the new indentation level, and
-- whether or not this line ended with a continuation token (in which case the
-- following line ought to be treated accordingly)
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
parse_indent =
  concat . process_indent . map normalize_line . break_lines

-- Display the tokens on a line.
show_line :: [Token] -> String
show_line [] = "\n"
show_line (x:xs) = show x ++ "\n" ++ show_line xs
