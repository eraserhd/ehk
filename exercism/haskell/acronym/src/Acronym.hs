module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)
import Data.List (tails)
import Data.Maybe (mapMaybe)

abbreviate :: String -> String
abbreviate = mapMaybe foo . tails . (' ' :)
  where
    foo (c1:c2:_)
      | not (isAlpha c1) && isAlpha c2 = Just $ toUpper c2
      | isLower c1 && isUpper c2       = Just c2
      | otherwise                      = Nothing
    foo _                              = Nothing
