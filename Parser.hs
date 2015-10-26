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

-- Run the lexer!
main = do
  chars <- getContents
  let raw_tokens = tokens read_token chars
  let tokens = parse_indent raw_tokens
  putStr . concat . map ((++"\n") . show) $ tokens
