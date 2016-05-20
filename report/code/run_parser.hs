-- Evaluation rules for parsers. This takes a parser, and a sequence of tokens,
-- and produces a list of possible parse results. Each result is either
-- a success, which means the subtree parsed and parsing can continue, or it is
-- a failure, in which case the subtree did not parse and it may be desirable to
-- display an error message.
--
-- Typical parsers produce a *lot* of redundant results, so it is *extremely*
-- beneficial to remove duplicates. This is why the requirement on Eq is
-- scattered all over the place, and is what 'nub' is doing.
run_parser :: (Eq a, Eq b, Show a) =>
              Parser (Token a) b -> [Token a] -> [Result (b, [Token a])]
run_parser p = nub . run_parser' p

-- Epsilon always successfully parses.
run_parser' :: (Eq a, Eq b, Show a) =>
              Parser (Token a) b -> [Token a] -> [Result (b, [Token a])]
run_parser' (Epsilon x) xs = [Success (x, xs)]

-- A matcher either fails, or succeeds with one parse.
run_parser' (Match m f) [] = [Failure EOF $ "Expected " ++ m]
run_parser' (Match m f) (x:xs) =
  if f x then
    [Success (x, xs)]
  else
    [Failure (token_location x)
             ("Unexpected " ++ show (token_type x))]

-- A union can succeed with any possible parses of any of its constituents.
run_parser' (Union ps) xs = concat . transpose . map (flip run_parser xs) $ ps

-- A concatenation can succeed only for successful parses of the second
-- immediately following any successful parse of the first.
run_parser' (Concat a b) xs =
  run_parser a xs >>= (\result ->
    case result of
      Failure l m -> [Failure l m]
      Success (a', xs') ->
        map (fmap (\(b', xs'') -> ((a', b'), xs''))) $ run_parser b xs')

-- A Klein star can succeed for any number of repetitions of p (even 0).
-- This can never fail, and success gives all matching prefixes.
run_parser' (Star p) xs =
  run_parser p xs >>= (\result ->
    case result of
      Failure l m -> [Success ([], xs), Failure l m]
      Success (a', xs') ->
        run_parser (Star p) xs' >>= (\result' ->
          [result' >>= (\(as', xs'') -> return ((a' : as', xs''))),
           Success ([], xs)]))

-- A reduction succeeds directly when the reduced parser succeeds.
run_parser' (Reduce f p) xs =
  run_parser p xs >>= return . (fmap (\(a, xs') -> (f a, xs')))

-- Run a parser and take the first parse which consumes all input.
full_parse :: (Eq a, Eq b, Show a, Show b) =>
              Parser (Token a) b -> [Token a] -> b
full_parse p xs =
  let ps = run_parser p xs in
    case [full | Success (full, []) <- ps] of
      [] -> error . show $ failure ps
      (x:xs) -> x
