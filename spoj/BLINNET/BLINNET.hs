{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Data.Char ( isDigit )
import Data.Array
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BS

type UnionFind = Array Int Int

unionInit :: Int -> UnionFind
unionInit n = array (0, n - 1) [(i, i) | i <- [0..n-1]]

unionFind :: UnionFind -> Int -> (UnionFind, Int)
unionFind uf i = let j = uf ! i in
                 if i == j
                 then (uf, i)
                 else let (uf2, k) = unionFind uf j
                      in ((uf2 // [(i, k)]), k)

unionMerge :: UnionFind -> Int -> Int -> (UnionFind, Bool)
unionMerge uf1 a b = let (uf2, a') = unionFind uf1 a
                         (uf3, b') = unionFind uf2 b
                     in if a' /= b'
                        then (uf3 // [(a', b')], True)
                        else (uf3, False)

numbers :: BS.ByteString -> [Int]
numbers = map (read . BS.unpack) . filter (isDigit . BS.head) . BS.words

parseN :: ([Int] -> ([Int], a)) -> [Int] -> ([Int], [a])
parseN f (n : ints) = mapAccumL (\a _ -> f a) ints [1..n]

parseCases = parseN parseCase
parseCase = parseN parseCity
parseCity = parseN parseEdge
parseEdge (neigh : cost : ints) = (ints, (neigh - 1, cost))

edges cities = nub $ sort $ concat $ zipWith (\n city -> map (\(neigh, cost) -> (cost, min n neigh, max n neigh)) city) [0..] cities

type State = (Int, UnionFind)

solve cities = fst $ foldl takeEdge start (edges cities)
               where
                 n = length cities

                 start :: State
                 start = (0, unionInit n)

                 takeEdge :: State -> (Int, Int, Int) -> State
                 takeEdge (totalCost, uf) (cost, from, to) =
                   case unionMerge uf from to of
                     (uf', True)  -> (totalCost + cost, uf')
                     (uf', False) -> (totalCost, uf')

main = do
  ints <- numbers `fmap` BS.getContents
  mapM_ (putStrLn . show . solve) $ snd $ parseCases ints
