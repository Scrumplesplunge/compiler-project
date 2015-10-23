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

token_type :: Token a -> a
token_type (Token t _) = t

token_location :: Token a -> Location
token_location (Token _ l) = l

-- Convert a token type and state into a token.
as_token :: (a, InputState) -> Maybe (Token a, InputState)
as_token (token_type, state) = Just (Token token_type $ location state, state)

-- Match an exact token type.
match_token :: a -> String -> Reader (Token a)
match_token t value = Reader (\input ->
  run_reader (match t value) input >>= as_token)

match_and_finish :: Reader a -> (a -> b) -> Reader (Token b)
match_and_finish read f = Reader (\input ->
  run_reader (read >>= (return . f)) input >>= as_token)

tokens :: Reader (Token a) -> String -> [Token a]
tokens reader input = tokens' $ new_state input
  where tokens' (InputState [] _) = []
        tokens' input' =
          case run_reader (repeat0 reader) input' of
            Nothing -> error "This should never happen."
            Just (xs, input'') -> xs

-- Keywords.
match_keyword keyword = match_token (KEYWORD keyword) (show keyword)
read_keyword =
  map match_keyword [
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
    (SUB,          "-")]

-- String/char literals.
match_char = first_of [
    all_of [match_filter ((/=) '*')],
    all_of [match_filter ((==) '*'), match_filter (const True)]]

read_char =
  match_and_finish (all_of [match "'" "'", match_char, match "'" "'"])
                   (CHAR . concat)
read_string =
  match_and_finish (all_of [
    all_of [match "\"" "\""],
    repeat0 match_char,
    all_of [match "\"" "\""]]) (STRING . concat . map concat)

-- Other tokens.
read_comment =
  match_and_finish
    (read_cons (match '-' "-")
               (read_cons (match '-' "-")
                          (repeat0 (match_filter ((/=) '\n')))))
    COMMENT
read_ident =
  match_and_finish
    (read_cons (match_filter isAlpha) (repeat0 $ match_filter identDigit))
    IDENT
  where identDigit x = isAlphaNum x || x == '.'
read_integer = match_and_finish (repeat1 (match_filter isDigit))
                                (INTEGER . read)
read_newline = match_token NEWLINE      "\n"
read_spaces = match_and_finish (repeat1 (match () " ")) (SPACES . length)

-- Read a single occam token.
read_token = first_of (
    read_keyword ++ read_symbol ++ [
      read_char,
      read_comment,
      read_ident,
      read_integer,
      read_newline,
      read_string,
      read_spaces])

-- For debugging purposes: output each of the token types.
show_tokens :: Reader RawToken -> String -> IO ()
show_tokens read_token =
  putStr . concat . map (\(Token t (Location line char)) ->
    show line ++ ":" ++ show char ++ ":\t" ++ show t ++ "\n") .
  tokens read_token
