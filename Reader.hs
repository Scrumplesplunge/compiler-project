module Reader where

import Input

type Reader a = InputState -> Maybe (a, InputState)

-- Match an exact prefix.
match :: a -> String -> Reader a
match result value input = match' value input
  where match' [] input' = Just (result, input')
        match' (x:xs) input' =
          getch input' >>= (\(y, input'') ->
            if x == y then match' xs input'' else Nothing)

-- Match a character that passes a predicate
match_filter :: (Char -> Bool) -> Reader Char
match_filter f input =
  getch input >>= (\(y, input') ->
    if f y then Just (y, input') else Nothing)

-- Try each reader in sequence until one succeeds.
first_of :: [Reader a] -> Reader a
first_of [] state = Nothing
first_of (r:rs) state =
  case r state of
    Nothing -> first_of rs state
    j -> j

-- Match each reader in sequence, succeeding only if all succeed.
all_of :: [Reader a] -> Reader [a]
all_of [] state = Just ([], state)
all_of (r:rs) state =
  r state >>= (\(x, state') ->
    all_of rs state' >>= (\(xs, state'') ->
      Just (x:xs, state'')))

-- Read one element, followed by multiple elements.
read_cons :: Reader a -> Reader [a] -> Reader [a]
read_cons r rs state =
  r state >>= (\(x, state') ->
    rs state' >>= (\(xs, state'') ->
      Just (x:xs, state'')))

-- Repeat a reader until it fails. The resultant reader never fails.
repeat0 :: Reader a -> Reader [a]
repeat0 read input =
  case read input of
    Nothing -> Just ([], input)
    Just (x, input') ->
      repeat0 read input' >>= (\(xs, input'') ->
        Just (x:xs, input''))

-- Like repeat, but must succeed at least once.
repeat1 :: Reader a -> Reader [a]
repeat1 read input = read_cons read (repeat0 read) input
