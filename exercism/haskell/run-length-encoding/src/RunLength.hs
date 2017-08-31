module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode          :: String -> String
decode ""       = ""
decode text@(c : cs)
  | isDigit c = let ((n, letter : rest) : _) = reads text
                in replicate n letter ++ decode rest
  | otherwise = c : decode cs

encode :: String -> String
encode = foldMap encodeGroup . group
  where
    encodeGroup [c]        = [c]
    encodeGroup cs@(c : _) = show (length cs) ++ [c]
