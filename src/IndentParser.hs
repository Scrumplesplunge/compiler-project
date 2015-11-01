module IndentParser where

import Data.Char
import qualified Lexer
import qualified Tokens
import Reader

-- Takes a line and returns how much it is indented by.
indent_level :: [Lexer.RawToken] -> Int
indent_level [] = 0
indent_level (t:ts) =
  case Tokens.token_type t of
    Lexer.SPACES s -> s
    _ -> 0

-- Returns true if the line ends with a continuation token.
continues :: [Lexer.RawToken] -> Bool
continues [] = False
continues [t] = Lexer.is_continuation_token t
continues (s:t:ts) = continues (t:ts)

-- Break a sequence of raw tokens into separate lines, by splitting at the
-- newline tokens.
break_lines :: [Lexer.RawToken] -> [[Lexer.RawToken]]
break_lines = foldr f [[]]
  where f t (l:ls) =
          if Tokens.token_type t == Lexer.NEWLINE then
            []:l:ls
          else
            (t:l):ls

-- Strip all instances of SPACES / COMMENTS.
strip_spaces :: [Lexer.RawToken] -> [Lexer.RawToken]
strip_spaces [] = []
strip_spaces (x:xs) =
  case Tokens.token_type x of
    Lexer.SPACES _ -> strip_spaces xs
    Lexer.COMMENT _ -> strip_spaces xs
    _ -> x : strip_spaces xs

-- Normalisation of a line involves:
-- * Removing any whitespace that isn't indentation.
-- * Removing any comments.
normalize_line :: [Lexer.RawToken] -> [Lexer.RawToken]
normalize_line [] = []
normalize_line (x:xs) =
  case Tokens.token_type x of
    Lexer.SPACES _ -> x : strip_spaces xs
    _ -> strip_spaces (x:xs)

-- Token stream representation after indentation has been handled.
data TokenType = CHAR Char              -- <Character literal, processed>
               | DEDENT                 -- <decreased indentation>
               | IDENT String           -- 'x'
               | INDENT                 -- <increased indentation>
               | INTEGER Integer        -- '2'
               | KEYWORD Lexer.Keyword  -- <Any Keyword>
               | STRING String          -- <String literal, processed>
               | SYMBOL Lexer.Symbol    -- <Any symbol>
  deriving Eq

-- Prettier definition of show, for error messages.
instance Show TokenType where
  show (CHAR c) = "character literal " ++ show c
  show DEDENT = "dedent"
  show (IDENT i) = "identifier '" ++ i ++ "'"
  show INDENT = "indent"
  show (INTEGER i) = "integer '" ++ show i ++ "'"
  show (KEYWORD k) = "keyword '" ++ show k ++ "'"
  show (STRING s) = "string literal " ++ show s
  show (SYMBOL s) = "symbol " ++ show s

type Token = Tokens.Token TokenType

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
convert :: [Lexer.RawToken] -> [Token]
convert = map $ fmap f
  where f x =
          case x of
            Lexer.CHAR c    -> CHAR . head $ interpret_escapes c
            Lexer.COMMENT c -> error "Comment in normalized token stream."
            Lexer.IDENT i   -> IDENT i
            Lexer.INTEGER i -> INTEGER i
            Lexer.KEYWORD k -> KEYWORD k
            Lexer.NEWLINE   -> error "Newline in normalized token stream."
            Lexer.SPACES n  -> error "Whitespace in normalized token stream."
            Lexer.STRING s  -> STRING $ interpret_escapes s
            Lexer.SYMBOL s  -> SYMBOL s

-- Remove leading whitespace from continuation lines.
process_indent :: [[Lexer.RawToken]] -> [[Token]]
process_indent ls = process_indent' ls 0 False

-- Normalize a continuation line. That is, if the line ends with a continuation
-- token, remove the NEWLINE.
norm_cont :: Bool -> Int -> [[Lexer.RawToken]] -> [Token] -> [[Token]]
norm_cont c new_indent ls l =
  l : process_indent' ls new_indent c

-- Internals of process indent: Takes lines, indent, and whether or not to treat
-- the first line as a continuation line, and produces lines of tokens with
-- correct indentation.
process_indent' :: [[Lexer.RawToken]] -> Int -> Bool -> [[Token]]
process_indent' [] _ True = []
process_indent' [] previous_indent False =
  [indent_by Reader.start (-previous_indent)]
process_indent' (l:ls) previous_indent True =
  if indent_level l < previous_indent then
    let (Tokens.Token t loc) = head l in
      error $ "Badly indented continuation line at " ++ show loc
  else
    norm_cont (continues l) previous_indent ls $ convert $ strip_spaces l
process_indent' (l:ls) previous_indent False =
  let (new_indent, ts) = process_line previous_indent l in
    norm_cont (continues l) new_indent ls ts

-- Given a single line of RawTokens, and the indentation before it, produce the
-- corresponding line of Tokens as well as the new indentation level, and
-- whether or not this line ended with a continuation token (in which case the
-- following line ought to be treated accordingly)
process_line :: Int -> [Lexer.RawToken] -> (Int, [Token])
process_line indent [] = (indent, [])
process_line indent (x:xs) =
  case Tokens.token_type x of
    Lexer.SPACES s -> f s xs
    _ -> f 0 (x:xs)
  where l = Tokens.token_location x
        f :: Int -> [Lexer.RawToken] -> (Int, [Token])
        f n ys = (n, indent_by l (n - indent) ++ convert ys)

-- Indent (or dedent) by the given amount.
indent_by :: Location -> Int -> [Token]
indent_by loc 0 = []
indent_by loc 1 = error $ "Bad indentation at " ++ show loc
indent_by loc (-1) = error $ "Bad indentation at " ++ show loc
indent_by loc x =
  if x < 0 then
    Tokens.Token DEDENT loc : indent_by loc (x + 2)
  else
    Tokens.Token INDENT loc : indent_by loc (x - 2)

-- Turn a sequence of RawTokens into Tokens, by parsing the indentation.
parse_indent :: [Lexer.RawToken] -> [Token]
parse_indent = concat . process_indent . map normalize_line . break_lines
