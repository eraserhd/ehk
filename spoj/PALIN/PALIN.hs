{-# OPTIONS_GHC -O2 -optc-O2 #-}
import Control.Exception.Base (assert)
import qualified Data.ByteString.Lazy.Char8 as BS

nextPalindrome input =
  let p = palindrome input
      leftDigits n = let nLength = BS.length n
                     in BS.take ((div nLength 2) + (rem nLength 2)) n
      palindrome n = BS.append (leftDigits n) $ BS.reverse (BS.take (div (BS.length n) 2) n)
      larger = let Just (v, _) = BS.readInteger (leftDigits p)
                   left = BS.pack $ show (v + 1)
                   right = BS.take (div (BS.length p) 2) $ BS.repeat '0'
               in palindrome $ BS.append left right
  in if BS.length p > BS.length input || p > input
     then p
     else larger

tests = [
  ("8", "9"),
  ("12305", "12321"),
  ("12345" , "12421"),
  ("123456" , "124421"),
  ("123256" , "123321"),
  ("99" , "101"),
  ("999" , "1001"),
  ("123300" , "123321"),
  ("10" , "11"),
  ("9" , "11"),
  ("555" , "565"),
  ( take 1000000 (repeat '1'),
    take 499999 (repeat '1') ++ "22" ++ take 499999 (repeat '1') ),
  ( take 1000000 (repeat '9'),
    "1" ++ take 999999 (repeat '0') ++ "1" ),
  ( take 99999 (repeat '1'),
    take 49999 (repeat '1') ++ "2" ++ take 49999 (repeat '1') ) ]

test = foldr (\(i, o) r -> assert (nextPalindrome (BS.pack i) == BS.pack o) r) "All tests pass" tests

main = BS.interact (BS.unlines . map nextPalindrome . tail . BS.lines)
