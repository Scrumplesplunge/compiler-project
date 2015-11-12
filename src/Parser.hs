module Parser where

import AST
import Parsing
import IndentParser
import qualified Lexer
import qualified Tokens

-- Convenience function for matching token types.
match_type :: Eq a =>
              String -> (TokenType -> Bool) -> (TokenType -> a) -> Parser Token a
match_type name m f =
  Match name (m . Tokens.token_type) >>> (\t -> f (Tokens.token_type t))

-- TERMINAL PARSERS
char_literal :: Parser Token Char
char_literal = match_type "character literal"
  (\t -> case t of
    CHAR _ -> True
    _ -> False)
  (\t -> let (CHAR x) = t in x)

-- Decreased indentation.
dedent :: Parser Token ()
dedent = match_type "dedent" (==DEDENT) (const ())

-- Identifier.
ident :: Parser Token String
ident = match_type "identifier"
  (\t -> case t of
    IDENT _ -> True
    _ -> False)
  (\t -> let (IDENT x) = t in x)

-- Increased indentation.
indent :: Parser Token ()
indent = match_type "indent" (==INDENT) (const ())

-- Integer literal.
integer :: Parser Token Integer
integer = match_type "integer"
  (\t -> case t of
    INTEGER x -> True
    _ -> False)
  (\t -> let (INTEGER x) = t in x)

-- Arbitrary keyword.
keyword :: Lexer.Keyword -> Parser Token ()
keyword k = match_type (show k) (==(KEYWORD k)) (const ())

-- String literal.
string_literal :: Parser Token String
string_literal = match_type "string literal"
  (\t -> case t of
    STRING x -> True
    _ -> False)
  (\t -> let (STRING x) = t in x)

-- Arbitrary symbol.
symbol :: Lexer.Symbol -> Parser Token ()
symbol s = match_type (show s) (==(SYMBOL s)) (const ())

-- NONTERMINAL PARSERS

-- A sequence of argument expressions.
actual_args :: Parser Token [RValue]
actual_args = Epsilon []
          ||| rvalue +++
              Star (symbol Lexer.COMMA +++
                    rvalue >>> snd)                  >>> (\(a, as) -> a : as)

-- A single argument name with the corresponding type.
arg_decl :: Parser Token (VarType, String)
arg_decl = keyword Lexer.VALUE +++ ident             >>> (\(_, v) -> (VALUE, v))
       ||| keyword Lexer.CHAN +++ ident              >>> (\(_, v) -> (CHAN, v))

-- Assignment process.
assignment :: Parser Token Process
assignment =
  lvalue +++ symbol Lexer.ASSIGN +++ rvalue          >>> (\(a, (_, b)) ->
                                                           Assignment a b)

-- The OR of one or more conjunctions.
cond :: Parser Token Condition
cond = conjunction
   ||| conjunction +++ keyword Lexer.OR +++ cond     >>> (\(a, (_, b)) -> Or a b)

-- The AND of one or more simple expressions.
conjunction :: Parser Token Condition
conjunction =
       simple_cond
   ||| simple_cond +++ keyword Lexer.AND +++
       conjunction                                   >>> (\(a, (_, b)) -> And a b)

-- Occam DEF.
define_constant :: Parser Token Definition
define_constant =
  keyword Lexer.DEF +++ ident +++
  symbol Lexer.COMP_EQ +++ expr +++
  symbol Lexer.COLON                                 >>> (\(_, (name, (_, (e, _)))) ->
                                                           DefineConstant name e)

-- Occam PROC.
define_procedure :: Parser Token Definition
define_procedure =
  keyword Lexer.PROC +++ ident +++
  symbol Lexer.OPEN_PAREN +++ formal_args +++
  symbol Lexer.CLOSE_PAREN +++
  symbol Lexer.COMP_EQ +++ indent +++
  process +++ dedent +++ symbol Lexer.COLON          >>> (\(_, (name, (_, (args, (_, (_, (_, (body, (_, _))))))))) ->
                                                           DefineProcedure name args body)

-- Occam VAR/CHAN.
define_variable :: Parser Token Definition
define_variable =
      keyword Lexer.VAR +++ ident +++
      symbol Lexer.COLON                             >>> DefineVariable . fst . snd
  ||| keyword Lexer.CHAN +++ ident +++
      symbol Lexer.COLON                             >>> DefineChannel . fst . snd


-- Any of the above definitions.
definition :: Parser Token Process
definition = (   define_constant
             ||| define_procedure
             ||| define_variable) +++ process       >>> (\(d, p) -> Definition d p)

-- A sum of one or more products.
expr :: Parser Token Expr
expr = left
         prod
         (symbol Lexer.ADD +++ prod                  >>> (\(_, b) -> (Lexer.ADD, b))
         ||| symbol Lexer.SUB +++ prod               >>> (\(_, b) -> (Lexer.SUB, b)))
         (\a (t, b) ->
           case t of
             Lexer.ADD -> Add a b
             Lexer.SUB -> Sub a b)

-- A sequence of argument names with their types.
formal_args :: Parser Token [(VarType, String)]
formal_args = Epsilon []
          ||| arg_decl +++
              Star (symbol Lexer.COMMA +++
                    arg_decl >>> snd)                >>> (\(a, as) -> a : as)

-- Read from a channel.
input :: Parser Token Process
input = rvalue +++ symbol Lexer.INPUT +++ lvalue     >>> (\(a, (_, b)) ->
                                                           Input a b)

-- Something which can be assigned to.
lvalue :: Parser Token LValue
lvalue = ident                                       >>> LVariable

-- Write to a channel.
output :: Parser Token Process
output = lvalue +++ symbol Lexer.OUTPUT +++
         rvalue                                      >>> (\(a, (_, b)) ->
                                                           Output a b)

-- Occam PAR.
parallel_block :: Parser Token Process
parallel_block =
      keyword Lexer.PAR +++ indent +++
      Star process +++ dedent                        >>> (\(_, (_, (ps, _))) ->
                                                           Parallel Nothing ps)
  ||| keyword Lexer.PAR +++ range +++ indent +++
      Star process +++ dedent                        >>> (\(_, (r, (_, (ps, _)))) ->
                                                           Parallel r ps)

-- Call an occam PROC.
procedure_call :: Parser Token Process
procedure_call =
  ident +++ symbol Lexer.OPEN_PAREN +++
  actual_args +++ symbol Lexer.CLOSE_PAREN           >>> (\(name, (_, (as, _))) ->
                                                           Call name as)

-- Arbitrary Occam process.
process :: Parser Token Process
process = keyword Lexer.SKIP                         >>> const Skip
      ||| keyword Lexer.STOP                         >>> const Stop
      ||| assignment
      ||| input
      ||| output
      ||| sequence_block
      ||| parallel_block
      ||| procedure_call
      ||| while_stmt
      ||| definition

-- A product of one or more terms.
prod :: Parser Token Expr
prod = left
         term
         (symbol Lexer.MUL +++ term                  >>> (\(_, b) ->
                                                           (Lexer.MUL, b))
         ||| symbol Lexer.DIV +++ term               >>> (\(_, b) ->
                                                           (Lexer.DIV, b)))
         (\a (t, b) ->
           case t of
             Lexer.MUL -> Mul a b
             Lexer.DIV -> Div a b)

-- For now, an Occam program is simply a single process.
program :: Parser Token Process
program = process

-- A range of the form <var> = [<expr> FOR <expr>].
range :: Parser Token Range
range =
  ident +++ symbol Lexer.OPEN_SQUARE +++ expr +++
  keyword Lexer.FOR +++ expr +++
  symbol Lexer.CLOSE_SQUARE                          >>> (\(a, (_, (b, (_, (c, _))))) ->
                                                           Just (a, b, c))

-- Something which can be assigned to variables.
rvalue :: Parser Token RValue
rvalue = expr                                        >>> Expr

-- Occam SEQ.
sequence_block :: Parser Token Process
sequence_block =
      keyword Lexer.SEQ +++ indent +++
      Star process +++ dedent                        >>> (\(_, (_, (ps, _))) ->
                                                           Sequence Nothing ps)
  ||| keyword Lexer.SEQ +++ range +++ indent +++
      Star process +++ dedent                        >>> (\(_, (r, (_, (ps, _)))) ->
                                                           Sequence r ps)

-- A simple condition expression.
simple_cond :: Parser Token Condition
simple_cond =
       keyword Lexer.TRUE                            >>> (\a -> Invariably True)
   ||| keyword Lexer.FALSE                           >>> (\a -> Invariably False)
   ||| expr +++ symbol Lexer.COMP_EQ +++ expr        >>> (\(a, (_, b)) -> CompareEQ a b)
   ||| expr +++ symbol Lexer.COMP_LT +++ expr        >>> (\(a, (_, b)) -> CompareLT a b)
   ||| expr +++ symbol Lexer.COMP_LE +++ expr        >>> (\(a, (_, b)) -> CompareLE a b)
   ||| expr +++ symbol Lexer.COMP_GT +++ expr        >>> (\(a, (_, b)) -> CompareGE a b)
   ||| expr +++ symbol Lexer.COMP_GE +++ expr        >>> (\(a, (_, b)) -> CompareGT a b)
   ||| expr +++ symbol Lexer.COMP_NE +++ expr        >>> (\(a, (_, b)) -> CompareNE a b)
   ||| keyword Lexer.NOT +++ simple_cond             >>> (\((), a) -> Not a)
   ||| symbol Lexer.OPEN_PAREN +++ cond +++
       symbol Lexer.CLOSE_PAREN                      >>> fst . snd

-- An atomic, but not necessarily terminal, expression.
term :: Parser Token Expr
term = ident                                         >>> RVariable
   ||| integer                                       >>> Literal . Integer
   ||| char_literal                                  >>> Literal . Char
   ||| string_literal                                >>> Literal . String
   ||| symbol Lexer.OPEN_PAREN +++ expr +++
       symbol Lexer.CLOSE_PAREN                      >>> fst . snd
   ||| symbol Lexer.SUB +++ term                     >>> Neg . snd

-- Unbounded loop.
while_stmt :: Parser Token Process
while_stmt =
  keyword Lexer.WHILE +++ cond +++ indent +++
  process +++ dedent                                 >>> (\(_, (c, (_, (p, _)))) ->
                                                           While c p)
