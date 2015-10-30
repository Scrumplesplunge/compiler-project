module Result where

import Reader

-- Parse result.
data Result a = Success a | Failure Location String
  deriving Eq

instance Show a => Show (Result a) where
  show (Success x) = show x
  show (Failure Unknown m) = "Failure: " ++ m
  show (Failure loc m) = "Failure: " ++ m ++ " at " ++ show loc

instance Functor Result where
  fmap f r = r >>= return . f

instance Monad Result where
  return x = Success x
  (Failure l m) >>= f = Failure l m
  (Success x) >>= f = f x

instance Applicative Result where
  pure = return
  rf <*> rx = rf >>= (\f -> fmap f rx)

(Failure l m) >>! xb = xb
(Success x) >>! xb = Success x

-- Some will succeed if at least one of the list succeeds. Otherwise, it returns
-- the value given in the second argument.
successes :: [Result a] -> Result [a]
successes [] = Failure Unknown "No options."
successes (Failure loc m : xs) =
  case successes xs of
    Failure loc' m' -> if loc' < loc then Failure loc m else Failure loc' m'
    Success as -> Success as
