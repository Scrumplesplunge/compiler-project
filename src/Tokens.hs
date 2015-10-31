module Tokens where

import Reader

data Token a = Token a Location
  deriving Eq

instance Show a => Show (Token a) where
  show (Token x loc) = show x

instance Functor Token where
  fmap f (Token x l) = Token (f x) l

token_type :: Token a -> a
token_type (Token t _) = t

token_location :: Token a -> Location
token_location (Token _ l) = l

-- Convert a token type and state into a token.
as_token :: Location -> a -> Token a
as_token loc token_type = Token token_type loc

-- Match an exact token type.
match_token :: a -> String -> Reader (Token a)
match_token t value =
  location >>= (\loc -> match t value >>= return . as_token loc)

match_and_finish :: Reader a -> (a -> b) -> Reader (Token b)
match_and_finish read f =
  location >>= (\loc -> read >>= return . as_token loc . f)

-- Repeatedly read tokens using the provided reader, and either return a list of
-- the tokens which make up the entire string, or return an error message about
-- the illegal token which prevents that from being the case.
tokens :: Reader (Token a) -> String -> [Token a]
tokens reader input = tokens' $ new_state input
  where tokens' (InputState [] _) = []
        tokens' input'@(InputState _ loc) =
          case run_reader reader input' of
            Nothing -> error $ "Illegal token at " ++ show loc 
            Just (x, input'') -> x : tokens' input''
