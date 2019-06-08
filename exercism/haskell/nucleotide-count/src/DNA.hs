module DNA (nucleotideCounts) where

import Control.Monad (foldM)
import qualified Data.Map as M

nucleotides :: [Char]
nucleotides = "ACGT"

emptyCount :: M.Map Char Int
emptyCount = M.fromList $ zip nucleotides $ repeat 0

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts = foldM addChar emptyCount
  where
    addChar :: M.Map Char Int -> Char -> Either String (M.Map Char Int)
    addChar m c
      | c `elem` nucleotides = Right $ M.alter (fmap (+1)) c m
      | otherwise            = Left $ "Bad nucleotide: " ++ [c]
