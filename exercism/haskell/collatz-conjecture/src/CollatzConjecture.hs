module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0         = Nothing
  | otherwise      = Just $ collatz' 0 n
  where
    collatz' x 1 = x
    collatz' x n
      | n `mod` 2 == 0 = collatz' (x + 1) (n `div` 2)
      | otherwise      = collatz' (x + 1) (3 * n + 1)
