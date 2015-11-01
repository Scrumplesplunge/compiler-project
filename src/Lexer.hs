module Lexer where

import Data.Char
import Reader
import Tokens

-- Tokenization.
data Keyword = CHAN
             | DEF
             | FALSE
             | FOR
             | IF
             | PAR
             | PROC
             | SEQ
             | SKIP
             | STOP
             | TRUE
             | VALUE
             | VAR
             | WHILE
  deriving (Eq, Show)

data Symbol = ADD           -- '+'
            | ASSIGN        -- ':='
            | CLOSE_PAREN   -- ')'
            | CLOSE_SQUARE  -- ']'
            | COLON         -- ':'
            | COMMA         -- ','
            | COMP_EQ       -- '='
            | COMP_GE       -- '>='
            | COMP_GT       -- '>'
            | COMP_LE       -- '<='
            | COMP_LT       -- '<'
            | COMP_NE       -- '<>'
            | DIV           -- '/'
            | INPUT         -- '?'
            | MUL           -- '*'
            | OPEN_PAREN    -- '('
            | OPEN_SQUARE   -- '['
            | OUTPUT        -- '!'
            | SEMICOLON     -- ';'
            | SUB           -- '-'
  deriving Eq

instance Show Symbol where
  show ADD = "'+'"
  show ASSIGN = "':='"
  show CLOSE_PAREN = "')'"
  show CLOSE_SQUARE = "']'"
  show COLON = "':'"
  show COMMA = "','"
  show COMP_EQ = "'='"
  show COMP_GE = "'>='"
  show COMP_GT = "'>'"
  show COMP_LE = "'<='"
  show COMP_LT = "'<'"
  show COMP_NE = "'<>'"
  show DIV = "'/'"
  show INPUT = "'?'"
  show MUL = "'*'"
  show OPEN_PAREN = "'('"
  show OPEN_SQUARE = "'['"
  show OUTPUT = "'!'"
  show SEMICOLON = "';'"
  show SUB = "'-'"

-- Raw token types include blocks of whitespace.
data RawType = CHAR String      -- <Character literal>
             | COMMENT String   -- '-- foo'
             | IDENT String     -- 'x'
             | INTEGER Integer  -- '2'
             | KEYWORD Keyword  -- <Any keyword>
             | NEWLINE          -- '\n'
             | SPACES Int       -- <at least one space>
             | STRING String    -- <String literal>
             | SYMBOL Symbol    -- <Any symbol>
  deriving (Eq, Show)

type RawToken = Token RawType

is_continuation_token :: RawToken -> Bool
is_continuation_token (Token t l) =
  case t of
    SYMBOL ADD -> True
    SYMBOL SUB -> True
    SYMBOL MUL -> True
    SYMBOL DIV -> True
    SYMBOL COMMA -> True
    SYMBOL SEMICOLON -> True
    SYMBOL ASSIGN -> True
    KEYWORD FOR -> True
    _ -> False

-- Keywords.
match_keyword keyword = match_token (KEYWORD keyword) (show keyword)
read_keyword = first_of $ map match_keyword [
    CHAN, DEF, FALSE, FOR, IF, PAR, PROC, SEQ, SKIP, STOP, TRUE, VAR, VALUE,
    WHILE]

-- Symbols.
match_symbol (symbol, value) = match_token (SYMBOL symbol) value
read_symbol =
  map match_symbol [
    (ADD,          "+"),
    (ASSIGN,       ":="),
    (CLOSE_PAREN,  ")"),
    (CLOSE_SQUARE, "]"),
    (COLON,        ":"),
    (COMMA,        ","),
    (COMP_EQ,      "="),
    (COMP_GE,      ">="),
    (COMP_GT,      ">"),
    (COMP_LE,      "<="),
    (COMP_LT,      "<"),
    (COMP_NE,      "<>"),
    (DIV,          "/"),
    (INPUT,        "?"),
    (MUL,          "*"),
    (OPEN_PAREN,   "("),
    (OPEN_SQUARE,  "["),
    (OUTPUT,       "!"),
    (SEMICOLON,    ";"),
    (SUB,          "-")]

-- String/char literals.
match_char = first_of [
    all_of [match_filter ((/=) '*')],
    all_of [match_filter ((==) '*'), match_filter (const True)]]

read_char =
  match_and_finish
    (all_of [match "'" "'", match_char, match "'" "'"])
    (CHAR . concat)

read_string =
  match_and_finish
    (all_of [match ["\""] "\"", repeat0 match_char, match ["\""] "\""])
    (STRING . concat . map concat)

-- Other tokens.
read_comment =
  match_and_finish
    (all_of [match "-" "-", match "-" "-", repeat0 (match_filter (/='\n'))])
    (COMMENT . concat)

read_ident_or_keyword =
  match_and_finish
    (read_cons (match_filter isAlpha) (repeat0 $ match_filter identDigit))
    (\x -> case run_reader read_keyword (new_state x) of
             Just (Token (KEYWORD k) loc, InputState [] _) -> KEYWORD k
             _ -> IDENT x)
  where identDigit x = isAlphaNum x || x == '.'

read_integer =
  match_and_finish
    (repeat1 (match_filter isDigit))
    (INTEGER . read)

read_newline = match_token NEWLINE "\n"
read_spaces = match_and_finish (repeat1 (match () " ")) (SPACES . length)

-- Read a single occam token.
read_token = first_of ([
      read_char,
      read_comment,
      read_ident_or_keyword,
      read_integer,
      read_newline,
      read_string,
      read_spaces] ++ read_symbol)
