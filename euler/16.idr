module Main

powerDigitSum : Int
powerDigitSum = foldl (+) 0 digits
                where
		  digitValue : Char -> Int
		  digitValue c = (ord c) - (ord '0')

                  digits : List Int
		  digits = map digitValue $ unpack $ show $ pow 2 1000

main : IO ()
main = putStrLn $ show powerDigitSum
