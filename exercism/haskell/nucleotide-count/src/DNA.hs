module DNA (nucleotideCounts) where

import Control.Monad (foldM)
import qualified Data.Map as M

nucleotides :: [Char]
nucleotides = "ACGT"

emptyCount :: M.Map Char Int
emptyCount = foldr (\k m -> M.insert k 0 m) M.empty nucleotides

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts = foldM addChar emptyCount
  where
    addChar :: M.Map Char Int -> Char -> Either String (M.Map Char Int)
    addChar m c
      | c `elem` nucleotides = Right $ M.alter (fmap (+1)) c m
      | otherwise            = Left $ "Bad nucleotide: " ++ [c]
