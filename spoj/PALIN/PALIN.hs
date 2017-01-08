import Control.Exception.Base (assert)
 
half n = div (length n) 2

extra n = rem (length n) 2

leftDigits n = take ((half n) + (extra n)) n

rightDigits n = reverse $ take (half n) n

nearest n = leftDigits n ++ rightDigits n

larger palindrome = let oldLeft = leftDigits palindrome
                        left = show ((read oldLeft :: Integer) + 1)
                        right = take (half palindrome) $ repeat '0'
                    in nearest (left ++ right)

nextPalindrome input = if (read palindrome :: Integer) > (read input :: Integer)
                       then palindrome
                       else larger palindrome
                       where
                         palindrome = nearest input

test = assert (nextPalindrome "8" == "9") $
       assert (nextPalindrome "12305" == "12321") $
       assert (nextPalindrome "12345" == "12421") $
       assert (nextPalindrome "123456" == "124421") $
       assert (nextPalindrome "123256" == "123321") $
       assert (nextPalindrome "99" == "101") $
       assert (nextPalindrome "999" == "1001") $
       assert (nextPalindrome "123300" == "123321") $
       assert (nextPalindrome "10" == "11") $
       assert (nextPalindrome "9" == "11") $
       assert (nextPalindrome "555" (nextPalindrome millionOnes == onetwotwooneyadayada ) $
       "Tests passed"
       where
         millionOnes = take 1000000 (repeat '1')
         onetwotwooneyadayada = take 499999 (repeat '1') ++ "22" ++ take 499999 (repeat '1')

main = interact (unlines . map nextPalindrome . tail . lines)
