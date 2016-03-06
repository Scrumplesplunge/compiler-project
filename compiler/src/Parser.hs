module Parser where

import AST
import Parsing
import Prelude hiding (sequence)
import IndentParser
import qualified Lexer
import qualified Tokens

-- Convenience function for matching token types.
match_type :: Eq a =>
              String -> (TokenType -> Bool) -> (TokenType -> a) -> Parser Token (L a)
match_type name m f =
  Match name (m . Tokens.token_type) >>> (\(Tokens.Token t loc) -> L (f t) loc)

-- TERMINAL PARSERS
char :: Parser Token (L Char)
char = match_type "character literal" matcher reducer
  where matcher (CHAR _) = True
        matcher _ = False
        reducer (CHAR x) = x
        reducer _ = error "char::reducer applied to non-char."

-- Decreased indentation.
dedent :: Parser Token (L ())
dedent = match_type "dedent" (==DEDENT) (const ())

-- Identifier.
name :: Parser Token (L String)
name = match_type "identifier" matcher reducer
  where matcher (IDENT _) = True
        matcher _ = False
        reducer (IDENT x) = x

-- Increased indentation.
indent :: Parser Token (L ())
indent = match_type "indent" (==INDENT) (const ())

-- Integer literal.
numeral :: Parser Token (L Integer)
numeral = match_type "integer" matcher reducer
  where matcher (INTEGER x) = True
        matcher _ = False
        reducer (INTEGER x) = x

-- Arbitrary keyword.
keyword :: Lexer.Keyword -> Parser Token (L ())
keyword k = match_type (show k) (==(KEYWORD k)) (const ())

-- String literal.
string :: Parser Token (L String)
string = match_type "string literal" matcher reducer
  where matcher (STRING x) = True
        matcher _ = False
        reducer (STRING x) = x

-- Arbitrary symbol.
symbol :: Lexer.Symbol -> Parser Token (L ())
symbol s = match_type (show s) (==(SYMBOL s)) (const ())

-- List of at least one p, separated by q's.
list :: (Eq a, Eq b) => Parser Token a -> Parser Token b -> Parser Token [a]
list p q = p +++ Star (q +++ p >>> snd)                                         >>> uncurry (:)

-- Comma-separated sequence.
comma_separated :: Eq a => Parser Token a -> Parser Token [a]
comma_separated p = list p $ symbol Lexer.COMMA

-- Parsers for the helper constructs in the AST.
nestable :: (Eq a, Eq b) => Parser Token (L a) -> Parser Token (L b) -> Parser Token (Nestable (L a) (L b))
nestable p q = p                                                                >>> Nested
           ||| q +++ indent +++
                 process +++ dedent                                             >>> (\(a, (_, (p, _))) -> Block a p)

replicable :: Eq a => Parser Token a -> Parser Token (Replicable a)
replicable p = indent +++
                 Star p +++ dedent                                              >>> (\(_, (ps, _)) -> Basic ps)
           ||| replicator +++ indent +++
                 p +++ dedent                                                   >>> (\(r, (_, (p, _))) -> Replicated r p)

replicator :: Parser Token Replicator
replicator = name +++ symbol Lexer.COMP_EQ +++ symbol Lexer.OPEN_SQUARE +++
             expression +++ keyword Lexer.FOR +++ expression +++
             symbol Lexer.CLOSE_SQUARE                                          >>> (\(n, (_, (_, (a, (_, (b, _)))))) -> Range n a b)

-- NONTERMINAL PARSERS

process :: Parser Token (L Process)
process = action
      ||| keyword Lexer.PRI +++ alternative                                     >>> fmap PriorityAlt . snd
      ||| alternative                                                           >>> fmap Alt
      ||| definitions
      ||| conditional                                                           >>> fmap If
      ||| keyword Lexer.PRI +++ parallel                                        >>> fmap PriorityPar . snd
      ||| parallel                                                              >>> fmap Par
      ||| sequence                                                              >>> fmap Seq
      ||| repetition

action :: Parser Token (L Process)
action = assign
     ||| call
     ||| delay
     ||| input
     ||| output
     ||| skip
     ||| stop
     ||| timer

-- List of at least two p's, separated by q's. This is to stop ambiguity in
-- expression, between the associative operator lists.
list2 :: (Eq a, Eq b) => Parser Token a -> Parser Token b -> Parser Token [a]
list2 p q = p +++ q +++ p +++ Star (q +++ p >>> snd)                            >>> (\(a, (b, (c, ds))) -> a : c : ds)

-- Most of the rules for expression start with the rule 'operand', which may be
-- arbitrarily expensive to check, and leads to a large amount of backtracking.
-- By replacing all these rules with one rule which reads the operand, followed
-- by a choice between all the available tails, the performance is significantly
-- improved.
data ExprType = OPERAND
              | ADD [L Expression]
              | AFTER (L Expression)
              | AND [L Expression]
              | BITWISE_AND [L Expression]
              | BITWISE_OR [L Expression]
              | BITWISE_XOR [L Expression]
              | COMP_EQ (L Expression)
              | COMP_GE (L Expression)
              | COMP_GT (L Expression)
              | COMP_LE (L Expression)
              | COMP_LT (L Expression)
              | COMP_NE (L Expression)
              | DIV (L Expression)
              | MOD (L Expression)
              | MUL [L Expression]
              | OR [L Expression]
              | SHIFT_LEFT (L Expression)
              | SHIFT_RIGHT (L Expression)
              | SUB (L Expression)
  deriving Eq

finish_expression :: (L Expression, ExprType) -> L Expression
finish_expression (a, expr_type) =
  case expr_type of
    OPERAND        -> a
    ADD bs         -> L (Add (a : bs))        (location a)
    AFTER b        -> L (After a b)           (location a)
    AND bs         -> L (And (a : bs))        (location a)
    BITWISE_AND bs -> L (BitwiseAnd (a : bs)) (location a)
    BITWISE_OR bs  -> L (BitwiseOr (a : bs))  (location a)
    BITWISE_XOR bs -> L (BitwiseXor (a : bs)) (location a)
    COMP_EQ b      -> L (CompareEQ a b)       (location a)
    COMP_GE b      -> L (CompareGE a b)       (location a)
    COMP_GT b      -> L (CompareGT a b)       (location a)
    COMP_LE b      -> L (CompareLE a b)       (location a)
    COMP_LT b      -> L (CompareLT a b)       (location a)
    COMP_NE b      -> L (CompareNE a b)       (location a)
    DIV b          -> L (Div a b)             (location a)
    MOD b          -> L (Mod a b)             (location a)
    MUL bs         -> L (Mul (a : bs))        (location a)
    OR bs          -> L (Or (a : bs))         (location a)
    SHIFT_LEFT b   -> L (ShiftLeft a b)       (location a)
    SHIFT_RIGHT b  -> L (ShiftRight a b)      (location a)
    SUB b          -> L (Sub a b)             (location a)

expression :: Parser Token (L Expression)
expression = symbol Lexer.SUB +++ operand                                       >>> (\(a, b) -> L (Neg b) (location a))
         ||| keyword Lexer.NOT +++ operand                                      >>> (\(a, b) -> L (Not b) (location a))
         ||| operand +++ (
                   Epsilon OPERAND
               ||| plus (symbol Lexer.ADD +++ operand >>> snd)                  >>> ADD
               ||| keyword Lexer.AFTER +++ operand                              >>> AFTER . snd
               ||| plus (keyword Lexer.AND +++ operand >>> snd)                 >>> AND
               ||| plus (symbol Lexer.BITWISE_AND +++ operand >>> snd)          >>> BITWISE_AND
               ||| plus (symbol Lexer.BITWISE_OR +++ operand >>> snd)           >>> BITWISE_OR
               ||| plus (symbol Lexer.BITWISE_XOR +++ operand >>> snd)          >>> BITWISE_XOR
               ||| symbol Lexer.COMP_EQ +++ operand                             >>> COMP_EQ . snd
               ||| symbol Lexer.COMP_GE +++ operand                             >>> COMP_GE . snd
               ||| symbol Lexer.COMP_GT +++ operand                             >>> COMP_GT . snd
               ||| symbol Lexer.COMP_LE +++ operand                             >>> COMP_LE . snd
               ||| symbol Lexer.COMP_LT +++ operand                             >>> COMP_LT . snd
               ||| symbol Lexer.COMP_NE +++ operand                             >>> COMP_NE . snd
               ||| symbol Lexer.DIV +++ operand                                 >>> DIV . snd
               ||| symbol Lexer.MOD +++ operand                                 >>> MOD . snd
               ||| plus (symbol Lexer.MUL +++ operand >>> snd)                  >>> MUL
               ||| plus (keyword Lexer.OR +++ operand >>> snd)                  >>> OR
               ||| symbol Lexer.SHIFT_LEFT +++ operand                          >>> SHIFT_LEFT . snd
               ||| symbol Lexer.SHIFT_RIGHT +++ operand                         >>> SHIFT_RIGHT . snd
               ||| symbol Lexer.SUB +++ operand                                 >>> SUB . snd
             )                                                                  >>> finish_expression

selector :: Parser Token (ArrayType, L Expression)
selector = symbol Lexer.OPEN_SQUARE +++ array_type +++ expression +++
           symbol Lexer.CLOSE_SQUARE                                            >>> (\(_, (t, (e, _))) -> (t, e))

slicer :: Parser Token (ArrayType, L Expression, L Expression)
slicer = symbol Lexer.OPEN_SQUARE +++ array_type +++ expression +++
         keyword Lexer.FOR +++ expression +++ symbol Lexer.CLOSE_SQUARE         >>> (\(_, (t, (a, (_, (b, _))))) -> (t, a, b))

data PartType = Selector (ArrayType, L Expression)
              | Slicer (ArrayType, L Expression, L Expression)
  deriving Eq

operand :: Parser Token (L Expression)
operand = left term
              (     selector >>> Selector
                ||| slicer   >>> Slicer)
              (\a b ->
                case b of
                  Selector s ->
                    L (Index a s) (location a)
                  Slicer s ->
                    L (Slice a s) (location a))

term :: Parser Token (L Expression)
term = name                                                                     >>> fmap Variable
   ||| numeral                                                                  >>> fmap (Literal . Integer)
   ||| keyword Lexer.TRUE                                                       >>> fmap (Literal . Bool . const True)
   ||| keyword Lexer.ANY                                                        >>> fmap (const Any)
   ||| keyword Lexer.FALSE                                                      >>> fmap (Literal . Bool . const False)
   ||| char                                                                     >>> fmap (Literal . Char)
   ||| string                                                                   >>> fmap (Literal . String)
   ||| keyword Lexer.TABLE +++ symbol Lexer.OPEN_SQUARE +++
       array_type +++ comma_separated expression +++ symbol Lexer.CLOSE_SQUARE  >>> (\(a, (_, (t, (es, _)))) ->
                                                                                      L (Literal (Table INT es)) (location a))
   ||| symbol Lexer.OPEN_PAREN +++ expression +++ symbol Lexer.CLOSE_PAREN      >>> fst . snd

array_type :: Parser Token ArrayType
array_type = keyword Lexer.BYTE                                                 >>> const BYTE
         ||| Epsilon ()                                                         >>> const INT

-- a := b
assign :: Parser Token (L Process)
assign = expression +++ symbol Lexer.ASSIGN +++ expression                      >>> (\(a, (_, b)) ->
                                                                                      L (Assign a b) (location a))

-- proc(a, b, c)
call :: Parser Token (L Process)
call = name                                                                     >>> (\(L n loc) ->
                                                                                      L (Call (L n loc) []) loc)
   ||| name +++ symbol Lexer.OPEN_PAREN +++ comma_separated expression +++
       symbol Lexer.CLOSE_PAREN                                                 >>> (\(L n loc, (_, (es, _))) ->
                                                                                      L (Call (L n loc) es) loc)

-- TIME ? AFTER x
delay :: Parser Token (L Process)
delay = keyword Lexer.TIME +++ symbol Lexer.INPUT +++ keyword Lexer.AFTER +++
        expression                                                              >>> (\(a, (_, (_, e))) ->
                                                                                      L (Delay e) (location a))

-- chan ? var1; var2; var3
input :: Parser Token (L Process)
input = expression +++ symbol Lexer.INPUT +++
        list expression (symbol Lexer.SEMICOLON)                                >>> (\(a, (_, bs)) ->
                                                                                      L (Seq (Basic (map (\b -> L (Input a b) (location a)) bs))) (location a))

-- chan ! expr1; expr2; expr3
output :: Parser Token (L Process)
output = expression +++ symbol Lexer.OUTPUT +++
         list expression (symbol Lexer.SEMICOLON)                               >>> (\(a, (_, bs)) ->
                                                                                      L (Seq (Basic (map (\b -> L (Output a b) (location a)) bs))) (location a))

-- SKIP
skip :: Parser Token (L Process)
skip = keyword Lexer.SKIP                                                       >>> fmap (const Skip)

-- STOP
stop :: Parser Token (L Process)
stop = keyword Lexer.STOP                                                       >>> fmap (const Stop)

-- TIME ? x
timer :: Parser Token (L Process)
timer = keyword Lexer.TIME +++ symbol Lexer.INPUT +++ expression                >>> (\(a, (_, e)) -> L (Timer e) (location a))

-- ALT
alternative :: Parser Token (L Alternative)
alternative = keyword Lexer.ALT +++ replicable (nestable alternative guard)     >>> (\(a, b) -> L (Alternative b) (location a))

guard :: Parser Token (L Guard)
guard = atomic_guard                                                            >>> (\g -> L (BasicGuard g) (location g))
    ||| expression +++ symbol Lexer.AMPERSAND +++ atomic_guard                  >>> (\(e, (_, g)) -> L (PrefixedGuard e g) (location e))

atomic_guard :: Parser Token (L AtomicGuard)
atomic_guard = expression +++ symbol Lexer.INPUT +++
               list expression (symbol Lexer.SEMICOLON)                         >>> (\(a, (_, bs)) -> L (InputGuard a bs) (location a))
           ||| keyword Lexer.TIME +++ symbol Lexer.INPUT +++
               keyword Lexer.AFTER +++ expression                               >>> (\(a, (_, (_, e))) -> L (DelayGuard e) (location a))
           ||| keyword Lexer.SKIP                                               >>> fmap (const SkipGuard)

-- Definitions

definitions :: Parser Token (L Process)
definitions = definition +++ symbol Lexer.COLON +++ process                     >>> (\(ds, (_, p)) ->
                                                                                      L (Definition ds p) (location (head ds)))

definition :: Parser Token [L Definition]
definition = keyword Lexer.CHAN +++ names CHAN                                  >>> snd
         ||| keyword Lexer.VAR +++ names VALUE                                  >>> snd
         ||| (keyword Lexer.CONST ||| keyword Lexer.DEF) +++ comma_separated (
               name +++ symbol Lexer.COMP_EQ +++ expression                     >>> (\(L a loc, (_, e)) -> L (DefineConstant a e) loc)
             )                                                                  >>> snd
         ||| procedure_definition
  where names t = comma_separated (
                        name +++ symbol Lexer.OPEN_SQUARE +++ expression +++
                        symbol Lexer.CLOSE_SQUARE                               >>> (\(L a loc, (_, (e, _))) -> L (DefineVector t a e) loc)
                    ||| name                                                    >>> (\(L a loc) -> L (DefineSingle t a) loc)
                  )

procedure_definition :: Parser Token [L Definition]
procedure_definition = keyword Lexer.PROC +++ name +++ formals +++
                       symbol Lexer.COMP_EQ +++ indent +++
                         process +++ dedent                                     >>> (\(L _ loc, (L n _, (fs, (_, (_, (p, _)))))) ->
                                                                                      [L (DefineProcedure n fs p) loc])

formals :: Parser Token [L Formal]
formals = symbol Lexer.OPEN_PAREN +++ comma_separated formal +++
          symbol Lexer.CLOSE_PAREN                                              >>> concat . fst . snd
      ||| Epsilon []

formal :: Parser Token [L Formal]
formal = keyword Lexer.CHAN +++ names CHAN                                      >>> snd
         -- TODO: Currently ignoring the distinction between CONST and VAR
         -- arguments.
     ||| keyword Lexer.VALUE +++ names VALUE                                    >>> snd
     ||| keyword Lexer.VAR +++ names VALUE                                      >>> snd
  where names t = comma_separated (
                        name +++ symbol Lexer.OPEN_SQUARE +++
                        symbol Lexer.CLOSE_SQUARE                               >>> (\(L a loc, (_, _)) -> L (Vector t a) loc)
                    ||| name                                                    >>> (\(L a loc) -> L (Single t a) loc)
                  )

-- IF
conditional :: Parser Token (L Condition)
conditional = keyword Lexer.IF +++
              replicable (nestable conditional expression)                      >>> (\(a, b) -> L (Condition b) (location a))

-- PAR
parallel :: Parser Token (L (Replicable (L Process)))
parallel = keyword Lexer.PAR +++ replicable process                             >>> (\(a, b) -> L b (location a))

-- SEQ
sequence :: Parser Token (L (Replicable (L Process)))
sequence = keyword Lexer.SEQ +++ replicable process                             >>> (\(a, b) -> L b (location a))

-- WHILE
repetition :: Parser Token (L Process)
repetition = keyword Lexer.WHILE +++ expression +++ indent +++
               process +++ dedent                                               >>> (\(a, (e, (_, (p, _)))) ->
                                                                                      L (While e p) (location a))
