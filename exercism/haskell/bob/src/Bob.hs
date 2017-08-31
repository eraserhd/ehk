module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)

responseFor :: String -> String
responseFor = responseForClean . filter (not . isSpace)
  where
    responseForClean :: String -> String
    responseForClean input
      | null input        = "Fine. Be that way!"
      | isYelling input   = "Whoa, chill out!"
      | last input == '?' = "Sure."
      | otherwise         = "Whatever."

    isYelling :: String -> Bool
    isYelling input = let letters = filter isAlpha input
                      in all isUpper letters && letters /= []
