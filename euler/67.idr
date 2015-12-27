module Main

parseTriangle : String -> List (List Int)
parseTriangle = map (map (cast {to=Int}) . words) . lines

red : List (List Int) -> List Int
red [l] = l
red (l1 :: l2 :: ls) = red $ added :: ls
                       where
                         lefts : List Int
                         lefts = l1 ++ [0]

                         rights : List Int
                         rights = [0] ++ l1

                         maxes : List Int
                         maxes = zipWith max lefts rights

                         added : List Int
                         added = zipWith (+) l2 maxes

main : IO ()
main = do Right text <- readFile "67.in"
          putStrLn $ show $ foldl max 0 $ red $ parseTriangle text
