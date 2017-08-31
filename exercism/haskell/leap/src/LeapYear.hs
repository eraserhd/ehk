module LeapYear (isLeapYear) where

import Control.Arrow

isLeapYear :: Integer -> Bool
isLeapYear y = divides 4 && (divides 400 || not (divides 100))
  where
    divides :: Integer -> Bool
    divides x = y `mod` x == 0

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
