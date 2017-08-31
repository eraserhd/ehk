module LeapYear (isLeapYear) where

import Control.Arrow

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `isDivisibleBy` 400 = True
  | year `isDivisibleBy` 100 = False
  | year `isDivisibleBy` 4   = True
  | otherwise                = False

isDivisibleBy :: Integer -> Integer -> Bool
x `isDivisibleBy` y = x `mod` y == 0

-- Point-free solution (not a nice solution, just for practice)
isLeapYear' :: Integer -> Bool
isLeapYear' = odd . length . filter (== 0) . flip (zipWith mod) [4, 100, 400] . repeat

-- Arrows solution (terrible solution, just for practice)
isLeapYear'' :: Integer -> Bool
isLeapYear'' = ((((divides 100 >>> not) &&& divides 400) >>> uncurry (||)) &&&
                divides 4) >>> uncurry (&&)
               where
                 divides :: (Arrow a, Integral n) => n -> a n Bool
                 divides x = arr (`mod` x) >>> arr (== 0)
