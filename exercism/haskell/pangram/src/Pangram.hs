module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (sort, nub, isInfixOf)

isPangram :: String -> Bool
isPangram = (['a'..'z'] `isInfixOf`) . nub . sort . map toLower
