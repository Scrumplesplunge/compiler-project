module Lexer where

import Data.Char
import Reader

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
  deriving (Eq, Show)

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

data Token a = Token a Location
  deriving Eq

instance Show a => Show (Token a) where
  show (Token x (Location l c)) =
    show l ++ ":" ++ show c ++ ": " ++ show x

instance Functor Token where
  fmap f (Token x l) = Token (f x) l

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

token_type :: Token a -> a
token_type (Token t _) = t

token_location :: Token a -> Location
token_location (Token _ l) = l

-- Convert a token type and state into a token.
as_token :: Location -> a -> Token a
as_token loc token_type = Token token_type loc

-- Match an exact token type.
match_token :: a -> String -> Reader (Token a)
match_token t value =
  location >>= (\loc -> match t value >>= return . as_token loc)

match_and_finish :: Reader a -> (a -> b) -> Reader (Token b)
match_and_finish read f =
  location >>= (\loc -> read >>= return . as_token loc . f)

-- Keywords.
match_keyword keyword = match_token (KEYWORD keyword) (show keyword)
read_keyword = map match_keyword [
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

read_ident =
  match_and_finish
    (read_cons (match_filter isAlpha) (repeat0 $ match_filter identDigit))
    IDENT
  where identDigit x = isAlphaNum x || x == '.'

read_integer =
  match_and_finish
    (repeat1 (match_filter isDigit))
    (INTEGER . read)

read_newline = match_token NEWLINE "\n"
read_spaces = match_and_finish (repeat1 (match () " ")) (SPACES . length)

-- Read a single occam token.
read_token = first_of (
    read_keyword ++ [
      read_char,
      read_comment,
      read_ident,
      read_integer,
      read_newline,
      read_string,
      read_spaces] ++ read_symbol)

-- Repeatedly read tokens using the provided reader, and either return a list of
-- the tokens which make up the entire string, or return an error message about
-- the illegal token which prevents that from being the case.
tokens :: Reader (Token a) -> String -> [Token a]
tokens reader input = tokens' $ new_state input
  where tokens' (InputState [] _) = []
        tokens' input'@(InputState _ loc) =
          case run_reader reader input' of
            Nothing -> error $ "Illegal token at " ++ show loc 
            Just (x, input'') -> x : tokens' input''
