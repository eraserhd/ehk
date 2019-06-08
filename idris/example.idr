module Example

import Data.So

-- Divisible by 3 "Fizz"
-- Divisible by 5 "Buzz"
-- Divisible by both, "FizzBuzz"
-- Neither -- just the number

%default total

-- SSID:     Alex's iPhone
-- Password: abcabcabca

isMod3 : Nat -> Bool
isMod3 Z             = True
isMod3 (S (S (S n))) = isMod3 n
isMod3 _             = False

isMod5 : Nat -> Bool
isMod5 Z                     = True
isMod5 (S (S (S (S (S n))))) = isMod5 n
isMod5 _                     = False

fizzBuzz : Nat -> String
fizzBuzz x with (isMod3 x, isMod5 x)
  fizzBuzz x | (True, True) = "FizzBuzz"
  fizzBuzz x | (True, False) = "Fizz"
  fizzBuzz x | (False, True) = "Buzz"
  fizzBuzz x | (False, False) = show x

foo : (n : Nat) -> Either (So (isMod3 n)) (So (isMod5 n)) -> Not (fizzBuzz n = "")
foo n (Left l)  = ?foo_rhs_1
foo n (Right r) = ?foo_rhs_2

{-
fizzIfDivisibleBy3 : (n : Nat) ->
                     So (isMod3 n && isMod5 n) ->
                     fizzBuzz n = "FizzBuzz"
fizzIfDivisibleBy3 n x with (choose (isMod3 n && isMod5 n))
  fizzIfDivisibleBy3 n x | (Left l) = ?fizzIfDivisibleBy3_rhs_1
  fizzIfDivisibleBy3 n x | (Right r) = ?fizzIfDivisibleBy3_rhs_2
-}


{-
fizzBuzz3ReturnsFizz : fizzBuzz 3 = "Fizz"
fizzBuzz3ReturnsFizz = Refl

fizzBuzz5ReturnsBuzz : fizzBuzz 5 = "Buzz"
fizzBuzz5ReturnsBuzz = Refl

fizzBuzz2Is2 : fizzBuzz 2 = "2"
fizzBuzz2Is2 = Refl

fizzBuzz15ReturnsFizzBuzz : fizzBuzz 15 = "FizzBuzz"
fizzBuzz15ReturnsFizzBuzz = Refl
-}
