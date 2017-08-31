module RunLength (decode, encode) where

import Control.Arrow ((&&&), (>>>))
import Data.Char (isDigit)
import Data.List (group)

decode          :: String -> String
decode ""       = ""
decode text@(c : cs)
  | isDigit c = let ((n, letter : rest) : _) = reads text
                in replicate n letter ++ decode rest
  | otherwise = c : decode cs

encode :: String -> String
encode = foldMap (length &&& head >>> encodeGroup) . group
  where
    encodeGroup (1, c) = [c]
    encodeGroup (n, c) = show n ++ [c]
