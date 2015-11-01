{-# LANGUAGE GADTs #-}

module Parsing where

import Data.List
import Reader
import Result
import Tokens

-- Generic parser rules.
data Parser a b where
  Epsilon :: b -> Parser a b
  Match :: String -> (a -> Bool) -> Parser a a
  Union :: [Parser a b] -> Parser a b
  Concat :: Parser a b -> Parser a c -> Parser a (b, c)
  Star :: Parser a b -> Parser a [b]
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
left :: Parser a b -> Parser a c -> (b -> c -> b) -> Parser a b
left p q f = Concat p (Star q) >>> (\(a, bs) -> foldl f a bs)

-- Evaluation rules for parsers. This takes a parser, and a sequence of tokens,
-- and produces a list of possible parse results. Each result is either
-- a success, which means the subtree parsed and parsing can continue, or it is
-- a failure, in which case the subtree did not parse and it may be desirable to
-- display an error message.
run_parser :: Show a =>
              Parser (Token a) b -> [(Token a)] -> [Result (b, [(Token a)])]

-- Epsilon always successfully parses.
run_parser (Epsilon x) xs = [Success (x, xs)]

-- A matcher either fails, or succeeds with one parse.
run_parser (Match m f) [] = [Failure EOF $ "Expected " ++ m]
run_parser (Match m f) (x:xs) =
  if f x then
    [Success (x, xs)]
  else
    [Failure (token_location x)
             ("Unexpected " ++ show (token_type x))]

-- A union can succeed with any possible parses of any of its constituents.
run_parser (Union ps) xs = concat . map (flip run_parser xs) $ ps

-- A concatenation can succeed only for successful parses of the second
-- immediately following any successful parse of the first.
run_parser (Concat a b) xs =
  run_parser a xs >>= (\result ->
    case result of
      Failure l m -> [Failure l m]
      Success (a', xs') ->
        map (fmap (\(b', xs'') -> ((a', b'), xs''))) $ run_parser b xs')

-- A Klein star can succeed for any number of repetitions of p (even 0).
-- This can never fail, and success gives all matching prefixes.
run_parser (Star p) xs =
  run_parser p xs >>= (\result ->
    case result of
      Failure l m -> [Success ([], xs), Failure l m]
      Success (a', xs') ->
        run_parser (Star p) xs' >>= (\result' ->
          return $ result' >>= (\(as', xs'') -> return ((a' : as', xs'')))))

-- A reduction succeeds directly when the reduced parser succeeds.
run_parser (Reduce f p) xs =
  run_parser p xs >>= return . (fmap (\(a, xs') -> (f a, xs')))

-- Run a parser and take the first parse which consumes all input.
full_parse :: Show a => Parser (Token a) b -> [Token a] -> b
full_parse p xs =
  let ps = run_parser p xs in
    case [full | Success (full, []) <- ps] of
      [] -> let (Failure l m) = failure ps in error . show $ (Failure l m :: Result ())
      (x:xs) -> x
