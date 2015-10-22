module Input where

-- Tracking the position in the source.
type LineNumber = Int
type CharNumber = Int
data Location = Location LineNumber CharNumber

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

-- Construct an input state.
new_state :: String -> InputState
new_state input = InputState input start

-- Read a single character and advance the input.
getch :: InputState -> Maybe (Char, InputState)
getch (InputState [] _) = Nothing
getch (InputState (x:xs) location) = Just (x, InputState xs $ update x location)

location :: InputState -> Location
location (InputState i l) = l
