--
--  BuzzFib (in Idris)
--
-- Write a program generating the first n Fibonacci numbers F(n), printing:
--
-- * "Buzz" when F(n) is divisible by 3.
-- * "Fizz" when F(n) is divisible by 5.
-- * "FizzBuzz" when F(n) is divisible by 15.
-- * "BuzzFizz" when F(n) is prime.
-- * the value F(n) otherwise.
--

import Data.Nat.DivMod
import Data.So

%default total -- Make sure our proofs are good

||| An inductive structure only constructible for valid Fibonacci numbers.
|||
||| Fib n x represents x, the nth Fibonacci number.
|||
data Fib : Nat -> Integer -> Type where
  Fib0 : Fib 0 1
  Fib1 : Fib 1 1
  FibN : Fib n x -> Fib (S n) y -> Fib (S (S n)) (x + y)

||| Proof: There is only one nth Fibonacci number
fibIsFunctional : Fib n x -> Fib n y -> x = y
fibIsFunctional Fib0 Fib0 = Refl
fibIsFunctional Fib1 Fib1 = Refl
fibIsFunctional (FibN a1 b1) (FibN a2 b2) with (fibIsFunctional a1 a2)
  fibIsFunctional (FibN a1 b1) (FibN a2 b2) | Refl with (fibIsFunctional b1 b2)
    fibIsFunctional (FibN a1 b1) (FibN a2 b2) | Refl | Refl = Refl

||| An infinite stream of fibonacci numbers.
data FibStream : Nat -> Type where
  (::) : Fib n x -> Inf (FibStream (S n)) -> FibStream n

fibs : FibStream 0
fibs = fibsN Fib0 Fib1
       where
         fibsN : Fib n x -> Fib (S n) y -> FibStream n
         fibsN a b = a :: fibsN b (FibN a b)

||| Types of output.
|||
||| This only adds a small bit of certainty in the accuracy of the program --
||| we could just have strings when defining the program semantics.
data Output = Buzz | Fizz | FizzBuzz | BuzzFizz | Literally Nat

implementation Show Output where
  show Buzz = "Buzz"
  show Fizz = "Fizz"
  show FizzBuzz = "FizzBuzz"
  show BuzzFizz = "BuzzFizz"
  show (Literally k) = show k

infixr 9 .|.

||| Does x divide y?
|||
||| divMod taked the predecessor of the divisor so that we can't represent
||| division by zero.
|||
||| Our divisibility and prime tests are boolean, (computational, runtime)
||| checks.  This is a bit sub-optimal for purposes of making verified
||| code due to "boolean blindness" - loosing the information contained in
||| propositions, so we can't use it elsewhere; however, making a type
||| inhabited solely by valid divisors and a type inhabited solely by primes
||| is quite a bit of work for this test.  (I started down that road, but
||| realized there's a lot to do to make proofs of modular congruences work.)
|||
||| assert_total is to tell Idris that (y `mod` x) always produces an answer
||| (since we handled the 0 case earlier).
(.|.) : Integer -> Integer -> Bool
0 .|. y = False
x .|. y = assert_total $ (y `mod` x) == 0

||| Pretty standard primality testing.
isPrime : Integer -> Bool
isPrime 1 = False
isPrime 2 = False
isPrime x = if 2 .|. x
            then False
            else assert_total $ noDivisorsOver 3
  where
    ||| Rather than convince Idris this terminates (which is hard, given
    ||| that we don't have access to the implementation of `>` and we
    ||| don't have a lot of theorems about Integer), we just declare this
    ||| partial.  :(
    partial
    noDivisorsOver : Integer -> Bool
    noDivisorsOver divisor = if divisor * divisor > x
                             then True
                             else if divisor .|. x
                                  then False
                                  else noDivisorsOver (divisor + 2)

{-
||| Our rules for output.
|||
||| Not (Prime x) in OutputBuzz and OutputFizz aren't part of the problem
||| statement, which is ambiguous in exactly the way we are using Idris to
||| prove we aren't, hence I've decided that 3 and 5 are treated as primes
||| rather than Buzz and Fizz, respectively.
data OutputSemantics : Nat -> Output -> Type where
  OutputBuzz      : Fib n x -> Not (Prime x) -> 3 .|. x -> Not (5 .|. x) -> OutputSemantics n Buzz
  OutputFizz      : Fib n x -> Not (Prime x) -> 5 .|. x -> Not (3 .|. x) -> OutputSemantics n Fizz
  OutputFizzBuzz  : Fib n x -> 15 .|. x -> OutputSemantics n FizzBuzz
  OutputBuzzFizz  : Fib n x -> Prime x -> OutputSemantics n BuzzFizz
  OutputLiterally : Fib n x -> Not (3 .|. x) -> Not (5 .|. x) -> Not (Prime x) -> OutputSemantics n (Literally x)


||| Proof: There is only one possible output for any n.
outputIsFunctional : OutputSemantics n output1 -> OutputSemantics n output2 -> output1 = output2

-}
