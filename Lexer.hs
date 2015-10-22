import Data.Char
import Input
import Reader

-- Tokenization.
data Keyword = FOR
             | DEF
             | IF
             | PAR
             | PROC
             | SEQ
             | SKIP
             | STOP
             | VAR
             | WHILE
  deriving Show

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
  deriving Show

data TokenType = ILLEGAL String   -- <Bad token>
               | CHAR String      -- <Character literal>
               | COMMENT String   -- '-- foo'
               | IDENT String     -- 'x'
               | INTEGER Integer  -- '2'
               | KEYWORD Keyword  -- <Any keyword>
               | NEWLINE          -- '\n'
               | SPACES Int       -- <at least one space>
               | STRING String    -- <String literal>
               | SYMBOL Symbol    -- <Any symbol>
  deriving Show

data Token = Token TokenType Location
  deriving Show

-- Convert a token type and state into a token.
as_token :: (TokenType, InputState) -> Maybe (Token, InputState)
as_token (token_type, state) = Just (Token token_type $ location state, state)

-- Match an exact token type.
match_token :: TokenType -> String -> Reader Token
match_token t value state = match t value state >>= as_token

match_and_finish :: Reader a -> (a -> TokenType) -> Reader Token
match_and_finish read f state =
  read state >>= (\(x, state') -> as_token (f x, state'))

fail_token :: String -> Location -> Token
fail_token message location =
  Token (ILLEGAL $ message ++ " at " ++ show location) location

tokens :: Reader Token -> String -> [Token]
tokens reader input = tokens' $ new_state input
  where tokens' (InputState [] _) = []
        tokens' state =
          case reader state of
            Nothing -> [fail_token "Bad token" $ location state]
            Just (x, state') -> x : tokens' state'

-- Keywords.
match_keyword keyword = match_token (KEYWORD keyword) (show keyword)
read_keyword =
  map match_keyword [FOR, DEF, IF, PAR, PROC, SEQ, SKIP, STOP, VAR, WHILE]

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
read_spaces state =
  repeat1 (match () " ") state >>= (\(xs, state') ->
    as_token (SPACES (length xs), state'))

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
show_tokens :: Reader Token -> String -> IO ()
show_tokens read_token =
  putStr . concat . map (\(Token t (Location line char)) ->
    show line ++ ":" ++ show char ++ ":\t" ++ show t ++ "\n") .
  tokens read_token

-- Run the lexer!
main = getContents >>= show_tokens read_token
