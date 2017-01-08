import Control.Exception.Base (assert)

nextPalindrome input =
  let p = palindrome input
      leftDigits n = let nLength = length n
                     in take ((div nLength 2) + (rem nLength 2)) n
      palindrome n = leftDigits n ++ reverse (take (div(length n) 2) n)
      larger = let left = show ((read (leftDigits p) :: Integer) + 1)
                   right = take (div (length p) 2) $ repeat '0'
               in palindrome (left ++ right)
  in if length p > length input || p > input
     then p
     else larger 

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
       assert (nextPalindrome "555" == "565") $
       assert (nextPalindrome millionOnes == lottaonestwotwolottaones) $
       assert (nextPalindrome millionNines == onelottazerosone) $
       assert (nextPalindrome nearlyHundredThousandOnes == lottaonestwolottaones) $
       "Tests passed"
       where
         nearlyHundredThousandOnes = take 99999 (repeat '1')
         lottaonestwolottaones = take 49999 (repeat '1') ++ "2" ++ take 49999 (repeat '1')
         millionOnes = take 1000000 (repeat '1')
         lottaonestwotwolottaones = take 499999 (repeat '1') ++ "22" ++ take 499999 (repeat '1')
         millionNines = take 1000000 (repeat '9')
         onelottazerosone = "1" ++ take 999999 (repeat '0') ++ "1"

main = interact (unlines . map nextPalindrome . tail . lines)
