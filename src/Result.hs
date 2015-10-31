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
-- the failure associated with the furthest location in the text.
successes :: [Result a] -> Result [a]
successes [] = Failure Unknown "No options."
successes (Failure loc m : xs) =
  case successes xs of
    Failure loc' m' -> if loc' < loc then Failure loc m else Failure loc' m'
    Success as -> Success as
successes (Success a : xs) =
  case successes xs of
    Failure _ _ -> Success [a]
    Success as -> Success (a:as)

-- Extract the rightmost failure.
failure :: [Result a] -> Result a
failure rs = failure' rs (Failure Unknown "Generic failure.")
  where failure' [] f = f
        failure' (Failure loc' m' : xs) (Failure loc m) =
          if loc' > loc then
            failure' xs (Failure loc' m')
          else
            failure' xs (Failure loc m)
