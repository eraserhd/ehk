module Hamming (distance) where

distance                   :: String -> String -> Maybe Int
distance [] []             = Just 0
distance (x : xs) (y : ys)
  | x == y                 = distance xs ys
  | otherwise              = (+ 1) <$> distance xs ys
distance _        _        = Nothing
