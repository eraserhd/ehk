module SumOfMultiples (sumOfMultiples) where

import Data.List (sort, nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum . nub . sort $ [ m | factor <- factors
                         , m <- [factor, 2*factor .. limit - 1] ]

