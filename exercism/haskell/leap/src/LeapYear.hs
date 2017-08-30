{-# LANGUAGE Arrows #-}

module LeapYear (isLeapYear) where

-- I'm using this problem to figure out Control.Arrow.  I make no claims that
-- arrows are a nice solution to this problem (they clearly aren't)...
import Control.Arrow

isLeapYear :: Integer -> Bool
isLeapYear = odd . length . filter (== 0) . flip (zipWith mod) [4, 100, 400] . repeat

{-
divides :: (Arrow a, Integral n) => n -> a n Bool
divides x = arr (`mod` x) >>> arr (== 0)

isLeapYear :: Integer -> Bool
isLeapYear = ((((divides 100 >>> not) &&& divides 400) >>> uncurry (||)) &&&
              divides 4) >>> uncurry (&&)
-}
