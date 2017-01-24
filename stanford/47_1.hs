digits pages = sum [ digitsForPlace place | place <- [0..7] ]
               where
                 pagesWithoutPlace place = 10 ^ place - 1
                 digitsForPlace place = max 0 (pages - pagesWithoutPlace place)

main = putStrLn $ show (head [ p | p <- [1..], digits p == 1890 ])
