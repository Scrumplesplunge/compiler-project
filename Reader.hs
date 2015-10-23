module Reader where

-- Tracking the position in the source.
type LineNumber = Int
type CharNumber = Int
data Location = Location LineNumber CharNumber
  deriving Eq

instance Show Location where
  show (Location line char) =
    "line " ++ show line ++ ", character " ++ show char

start :: Location
start = Location 1 1

line :: Location -> LineNumber
line (Location l c) = l

char :: Location -> CharNumber
char (Location l c) = c

update :: Char -> Location -> Location
update '\n' (Location l c) = Location (l + 1) 1
update _ (Location l c) = Location l (c + 1)

-- Reading the input.
data InputState = InputState String Location
  deriving Show

newtype Reader a = Reader (InputState -> Maybe (a, InputState))

-- Construct an input state.
new_state :: String -> InputState
new_state input = InputState input start

-- Read a single character and advance the input.
getch :: Reader Char
getch = Reader (\input ->
  case input of
    (InputState [] _) -> Nothing
    (InputState (x:xs) location) -> Just (x, InputState xs $ update x location))

run_reader (Reader r) input = r input

instance Functor Reader where
  fmap f ra = ra >>= return . f

instance Monad Reader where
  return x = Reader (Just . (,) x)
  (Reader xm) >>= f = Reader (\input ->
    xm input >>= (\(x, input') -> run_reader (f x) input'))

instance Applicative Reader where
  pure = return
  rab <*> ra = rab >>= (\f -> ra >>= return . f)

location :: Reader Location
location = Reader (\(InputState x l) -> Just (l, InputState x l))

read_fail :: Reader a
read_fail = Reader (const Nothing)

-- Bind failure.
infixl 1 >>!
(>>!) :: Reader a -> Reader a -> Reader a
(Reader ra) >>! (Reader rb) = Reader (\input ->
  case ra input of
    Nothing -> rb input
    j -> j)

-- Match an exact prefix.
match :: a -> String -> Reader a
match result value = match' value
  where match' [] = Reader (\input -> Just (result, input))
        match' (x:xs) =
          getch >>= (\y -> if x == y then match' xs else read_fail)

-- Match a character that passes a predicate
match_filter :: (Char -> Bool) -> Reader Char
match_filter f = getch >>= (\x -> if f x then return x else read_fail)

-- Try each reader in sequence until one succeeds.
first_of :: [Reader a] -> Reader a
first_of = foldr (>>!) read_fail
--r >>! first_of rs

-- Read one element, followed by multiple elements.
read_cons :: Reader a -> Reader [a] -> Reader [a]
read_cons r rs = r >>= (\x -> rs >>= (return . (x:)))

-- Match each reader in sequence, succeeding only if all succeed.
all_of :: [Reader a] -> Reader [a]
all_of = foldr read_cons (return [])

-- Repeat a reader until it fails. The resultant reader never fails.
repeat0 :: Reader a -> Reader [a]
repeat0 read = repeat1 read >>! return []

-- Like repeat, but must succeed at least once.
repeat1 :: Reader a -> Reader [a]
repeat1 read = read_cons read (repeat0 read)
