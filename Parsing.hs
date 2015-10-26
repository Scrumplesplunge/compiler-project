{-# LANGUAGE GADTs #-}

module Parsing where

import Data.List

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

-- Syntax sugar for unions.
infixr 4 |||
(Union ps) ||| (Union qs) = Union (ps ++ qs)
(Union ps) ||| q = Union (ps ++ [q])
p ||| (Union qs) = Union (p:qs)
p ||| q = Union [p, q]

-- Syntax sugar for reductions.
infixr 5 >>>
p >>> f = Reduce f p

-- Syntax sugar for concatenations.
infixr 6 +++
a +++ b = Concat a b

-- Handle left recursion: this works by turning:
--   parse_a = parse_b
--         ||| parse_a +++ parse_b     >>> f
-- Into:
--   parse_a = left
--               parse_b  -- base case
--               parse_b  -- repeated tail
--               f        -- reducer.
left :: Parser a b -> Parser a c -> ((b, c) -> b) -> Parser a b
left p q f = Union extensions
  where extensions = p : [e +++ q >>> f | e <- extensions]

repeat0 :: Parser a b -> Parser a [b]
repeat0 p = left
              (Epsilon >>> (const []))
              p
              (uncurry . flip $ (:))
              >>> reverse

-- Evaluation rules for parsers.
run_parser :: Parser a b -> [a] -> [(b, [a])]
run_parser Epsilon xs = [((), xs)]
run_parser (Match f) [] = []
run_parser (Match f) (x:xs) = if f x then [(x, xs)] else []
run_parser (Union ps) xs = concat . transpose . map (flip run_parser xs) $ ps
run_parser (Concat a b) xs =
  run_parser a xs >>= (\(a', xs') ->
    run_parser b xs' >>= (\(b', xs'') -> [((a', b'), xs'')]))
run_parser (Reduce f p) xs =
  run_parser p xs >>= (\(a, xs') -> [(f a, xs')])

-- Run a parser and take the first parse which consumes all input.
full_parse :: Parser a b -> [a] -> Maybe b
full_parse p xs =
  case [result | (result, []) <- run_parser p xs] of
    [] -> Nothing
    (x:xs) -> Just x
