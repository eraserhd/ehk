module Acronym (abbreviate) where

import Control.Arrow ((&&&))
import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = map (toUpper . snd) . filter p . uncurry zip . ((' ' :) &&& id)
  where
    p (c1, c2) = (not (isAlpha c1) && isAlpha c2) || (not (isUpper c1) && isUpper c2)
