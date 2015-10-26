{-# LANGUAGE GADTs #-}

import IndentParser
import Lexer

-- Parse result.
data Result a = Success a | Failure String
  deriving (Eq, Show)

instance Functor Result where
  fmap f r = r >>= return . f

instance Monad Result where
  return x = Success x
  (Failure m) >>= f = Failure m
  (Success x) >>= f = f x

instance Applicative Result where
  pure = return
  rf <*> rx = rf >>= (\f -> fmap f rx)

(Failure m) >>! xb = xb
(Success x) >>! xb = Success x

-- Generic parser rules.
data Parser a b where
  Epsilon :: Parser a ()
  Match :: (a -> Bool) -> Parser a a
  Union :: [Parser a b] -> Parser a b
  Concat :: Parser a b -> Parser a c -> Parser a (b, c)
  Reduce :: (b -> c) -> Parser a b -> Parser a c

infixr 4 |||
(Union ps) ||| (Union qs) = Union (ps ++ qs)
(Union ps) ||| q = Union (ps ++ [q])
p ||| (Union qs) = Union (p:qs)
p ||| q = Union [p, q]

infixr 5 >>>
p >>> f = Reduce f p

infixr 6 +++
a +++ b = Concat a b

run_parser :: Parser a b -> [a] -> [(b, [a])]
run_parser Epsilon xs = [((), xs)]
run_parser (Match f) [] = []
run_parser (Match f) (x:xs) = if f x then [(x, xs)] else []
run_parser (Union []) xs = []
run_parser (Union (p:ps)) xs = run_parser p xs ++ run_parser (Union ps) xs
run_parser (Concat a b) xs =
  run_parser a xs >>= (\(a', xs') ->
    run_parser b xs' >>= (\(b', xs'') -> [((a', b'), xs'')]))
run_parser (Reduce f p) xs =
  run_parser p xs >>= (\(a, xs') -> [(f a, xs')])

full_parse :: Parser a b -> [a] -> Maybe b
full_parse p xs =
  case [result | (result, []) <- run_parser p xs] of
    [] -> Nothing
    (x:xs) -> Just x

-- Parse an identifier.
ident = Match (\t ->
  case Lexer.token_type t of
    IndentParser.IDENT _ -> True
    _ -> False) >>> (\t ->
      case Lexer.token_type t of
        IndentParser.IDENT x -> x
        _ -> error "This should never happen.")

-- Parse an integer.
integer = Match (\t ->
  case Lexer.token_type t of
    IndentParser.INTEGER _ -> True
    _ -> False) >>> (\t ->
      case Lexer.token_type t of
        IndentParser.INTEGER x -> x
        _ -> error "This should never happen.")

-- Parse a symbol.
symbol :: Lexer.Symbol -> Parser IndentParser.Token Lexer.Symbol
symbol s = Match (\t ->
  case Lexer.token_type t of
    IndentParser.SYMBOL s' -> s == s'
    _ -> False) >>> const s

-- Parse a newline.
newline = Match (\t ->
  case Lexer.token_type t of
    IndentParser.NEWLINE -> True
    _ -> False) >>> const ()

-- Demo parser.
data AST = Add AST AST
         | Variable String
         | Value Integer

instance Show AST where
  show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Variable x) = x
  show (Value x) = show x

left :: Parser a b -> Parser a c -> ((b, c) -> b) -> Parser a b
left p q f = Union extensions
  where extensions = p : [e +++ q >>> f | e <- extensions]

line :: Parser IndentParser.Token AST
line = expr +++ newline                                               >>> fst

term :: Parser IndentParser.Token AST
term = ident                                                          >>> Variable
   ||| integer                                                        >>> Value
   ||| symbol Lexer.OPEN_PAREN +++ expr +++ symbol Lexer.CLOSE_PAREN  >>> (\(_, (e, _)) -> e)

expr :: Parser IndentParser.Token AST
expr = left
         term (symbol Lexer.ADD +++ term)                                 (\(a, (_, b)) -> Add a b)

-- Run the lexer!
main = do
  chars <- getContents
  let raw_tokens = tokens read_token chars
  let tokens = parse_indent raw_tokens
  putStr . concat . map ((++"\n") . show) $ tokens
  putStr "\n\n"
  putStr . show $ full_parse line tokens
