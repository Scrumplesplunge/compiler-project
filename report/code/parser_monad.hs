data Parser a b where
  Epsilon :: (Eq a, Eq b) => b -> Parser a b
  Match :: Eq a => String -> (a -> Bool) -> Parser a a
  Union :: (Eq a, Eq b) => [Parser a b] -> Parser a b
  Concat :: (Eq a, Eq b, Eq c) => Parser a b -> Parser a c -> Parser a (b, c)
  Star :: (Eq a, Eq b) => Parser a b -> Parser a [b]
  Reduce :: (Eq a, Eq b, Eq c) => (b -> c) -> Parser a b -> Parser a c
