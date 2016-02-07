module Generator where

type Label = String

data State = State { next_label_id :: Integer }

initial_state = State {
  next_label_id = 0
}

data Generator a = G (State -> (a, State))

instance Functor Generator where
  fmap f xm = xm >>= return . f

instance Applicative Generator where
  pure = return
  gf <*> gx = gf >>= (\f -> fmap f gx)

instance Monad Generator where
  return x = G (\state -> (x, state))
  (G xm) >>= f = G (\state -> do
    let (x, state') = xm state
    case f x of
      G xm' -> xm' state')

-- Run a generator and extract the final result.
run_generator :: Generator a -> a
run_generator (G f) = fst (f initial_state)

-- Retrieve a unique label containing the given string as a prefix.
label :: String -> Generator Label
label x = G (\state ->
  let id = next_label_id state
  in (x ++ "_" ++ show id, state { next_label_id = id + 1 }))
