import IndentParser
import qualified Lexer
import Parsing

-- Convenience function for matching token types.
match_type :: (TokenType -> Bool) -> (TokenType -> a) -> Parser Token a
match_type m f = Match (m . Lexer.token_type) >>> (\t -> f (Lexer.token_type t))

-- TERMINAL PARSERS
char_literal :: Parser Token Char
char_literal = match_type
  (\t -> case t of
    CHAR _ -> True
    _ -> False)
  (\t -> let (CHAR x) = t in x)

dedent :: Parser Token ()
dedent = match_type (==DEDENT) (const ())

ident :: Parser Token String
ident = match_type
  (\t -> case t of
    IDENT _ -> True
    _ -> False)
  (\t -> let (IDENT x) = t in x)

indent :: Parser Token ()
indent = match_type (==INDENT) (const ())

integer :: Parser Token Integer
integer = match_type
  (\t -> case t of
    INTEGER x -> True
    _ -> False)
  (\t -> let (INTEGER x) = t in x)

keyword :: Lexer.Keyword -> Parser Token ()
keyword k = match_type (==(KEYWORD k)) (const ())

string_literal :: Parser Token String
string_literal = match_type
  (\t -> case t of
    STRING x -> True
    _ -> False)
  (\t -> let (STRING x) = t in x)

symbol :: Lexer.Symbol -> Parser Token ()
symbol s = match_type (==(SYMBOL s)) (const ())

-- NONTERMINAL PARSERS
data AST = Add AST AST
         | Assignment AST AST
         | Char Char
         | Div AST AST
         | Input AST AST
         | Integer Integer
         | Mul AST AST
         | Output AST AST
         | Parallel [AST]
         | Sequence [AST]
         | Skip
         | Stop
         | String String
         | Sub AST AST
         | Variable String
  deriving Show

assignment :: Parser Token AST
assignment =
  ident +++ symbol Lexer.ASSIGN +++ expr             >>> (\(a, (_, b)) ->
                                                           Assignment (Variable a) b)

term :: Parser Token AST
term = ident                                         >>> Variable
   ||| integer                                       >>> Integer
   ||| char_literal                                  >>> Char
   ||| string_literal                                >>> String
   ||| symbol Lexer.OPEN_PAREN +++ expr +++
       symbol Lexer.CLOSE_PAREN                      >>> fst . snd

prod :: Parser Token AST
prod = left
  term
    (symbol Lexer.MUL +++ term                       >>> flip Mul . snd
    ||| symbol Lexer.DIV +++ term                    >>> flip Div . snd)
    (\(a, b) -> b a)

expr :: Parser Token AST
expr = left
        prod
        (symbol Lexer.ADD +++ prod                   >>> flip Add . snd
        ||| symbol Lexer.SUB +++ prod                >>> flip Sub . snd)
        (\(a, b) -> b a)

channel :: Parser Token AST
channel = ident                                      >>> Variable

input :: Parser Token AST
input = channel +++ symbol Lexer.INPUT +++ ident     >>> (\(a, (_, b)) ->
                                                           Input a (Variable b))

output :: Parser Token AST
output = channel +++ symbol Lexer.OUTPUT +++
         expr                                        >>> (\(a, (_, b)) ->
                                                           Output a b)

sequence_block :: Parser Token AST
sequence_block =
  keyword Lexer.SEQ +++ indent +++
  repeat0 process +++ dedent                         >>> (\((), ((), (ps, ()))) ->
                                                           Sequence ps)

parallel_block :: Parser Token AST
parallel_block =
  keyword Lexer.PAR +++ indent +++
  repeat0 process +++ dedent                         >>> (\((), ((), (ps, ()))) ->
                                                           Parallel ps)

process :: Parser Token AST
process = keyword Lexer.SKIP                         >>> const Skip
      ||| keyword Lexer.STOP                         >>> const Stop
      ||| assignment
      ||| input
      ||| output
      ||| sequence_block
      ||| parallel_block

-- Run the lexer!
main = do
  chars <- getContents
  let raw_tokens = Lexer.tokens Lexer.read_token chars
  let tokens = parse_indent raw_tokens
  putStrLn . concat . map ((++"\n") . show) $ tokens
  putStrLn . show $ full_parse process tokens
