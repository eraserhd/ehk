{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Data.Char ( isDigit )
import Data.Array
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BS

data UnionFind = UnionFind { unionLinks :: Array Int Int
                           , unionRanks :: Array Int Int
                           , unionSize  :: Array Int Int
                           }

unionInit :: Int -> UnionFind
unionInit n = UnionFind { unionLinks = array (0, n - 1) [(i, i) | i <- [0..n-1]]
                        , unionRanks = array (0, n - 1) [(i, 0) | i <- [0..n-1]]
                        , unionSize  = array (0, n - 1) [(i, 1) | i <- [0..n-1]]
                        }

unionFind :: UnionFind -> Int -> (UnionFind, Int)
unionFind uf i = let j = unionLinks uf ! i
                 in if i == j
                    then (uf, i)
                    else let (uf2, k) = unionFind uf j
                             uf3 = if k /= j
                                   then uf2 { unionLinks = (unionLinks uf2) // [(i, k)] }
                                   else uf2
                         in (uf3, k)

unionMerge :: UnionFind -> Int -> Int -> (UnionFind, Maybe Int)
unionMerge uf1 a b = if a' == b'
                     then (uf3, Nothing)
                     else case compare rankA rankB of
                            LT -> linkDifferentRanks a' b'
                            GT -> linkDifferentRanks b' a'
                            EQ -> linkEqualRanks
                     where
                       (uf2, a') = unionFind uf1 a
                       (uf3, b') = unionFind uf2 b
                       rankA = unionRanks uf3 ! a'
                       rankB = unionRanks uf3 ! b'
                       newSize = (unionSize uf3 ! a') + (unionSize uf3 ! b')

                       linkDifferentRanks lower higher =
                         (uf3 { unionLinks = (unionLinks uf3) // [(lower, higher)]
                              , unionSize  = (unionSize uf3)  // [(higher, newSize)] }, Just newSize )

                       linkEqualRanks =
                         (uf3 { unionLinks = (unionLinks uf3) // [(a', b')]
                              , unionSize  = (unionSize uf3)  // [(b', newSize)]
                              , unionRanks = (unionRanks uf3) // [(b', 1 + (unionRanks uf3 ! b') )] }, Just newSize )

numbers :: BS.ByteString -> [Int]
numbers = f . BS.dropWhile (not . isDigit)
          where
            f bs | BS.null bs = []
                 | otherwise  = let (num, rest) = BS.span isDigit bs
                                in (read $ BS.unpack num) : f (BS.dropWhile (not . isDigit) rest)


parseN :: ([Int] -> ([Int], a)) -> [Int] -> ([Int], [a])
parseN f (n : ints) = mapAccumL (\a _ -> f a) ints [1..n]

parseCases = parseN parseCase
parseCase = parseN parseCity
parseCity = parseN parseEdge
parseEdge (neigh : cost : ints) = (ints, (neigh - 1, cost))

edges = nub . sort . concat . zipWith (\n city -> map (\(neigh, cost) -> (cost, n, neigh)) $ filter ((>n) . fst) city) [0..]

type State = (Int, UnionFind, Bool)

solve cities = totalCost
               where
                 (totalCost, _, _) = build start $ edges cities

                 build st              []             = st
                 build st@(_, _, True) _              = st
                 build st              (edge : edges) = build (takeEdge st edge) edges

                 n = length cities

                 start :: State
                 start = (0, unionInit n, False)

                 takeEdge :: State -> (Int, Int, Int) -> State
                 takeEdge (totalCost, uf, _) (cost, from, to) =
                   case unionMerge uf from to of
                     (uf', Just newSize) -> (totalCost + cost, uf', newSize == n)
                     (uf', Nothing)      -> (totalCost, uf', False)

main = do
  ints <- numbers `fmap` BS.getContents
  mapM_ (putStrLn . show . solve) $ snd $ parseCases ints
