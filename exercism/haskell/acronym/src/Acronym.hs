module Acronym (abbreviate) where

import Control.Arrow ((&&&), (>>>))
import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = (' ' :) &&& id >>> uncurry zip >>> filter p >>> map (toUpper . snd)
  where
    p (c1, c2) = (not (isAlpha c1) && isAlpha c2) || (not (isUpper c1) && isUpper c2)
