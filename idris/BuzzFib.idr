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

%default total -- Make sure our proofs are good

||| An inductive structure only constructible for valid Fibonacci numbers.
|||
||| Fib n x represents x, the nth Fibonacci number.
|||
data Fib : Nat -> Nat -> Type where
  Fib0 : Fib 0 1
  Fib1 : Fib 1 1
  FibN : Fib n x -> Fib (S n) y -> Fib (S (S n)) (x + y)

||| Proof: There is only one nth Fibonacci number
fibIsFunctional : Fib n x -> Fib n y -> x = y

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

||| Only valid congurences (x = y (mod z)) are constructible.
|||
data ModularCongruence : (x, y, z : Nat) -> Type where
  ModularBase : x' `LTE` z' -> ModularCongruence x' x' (S z')
  ModularStep : ModularCongruence x' y' z' -> ModularCongruence (x' + z') y' z'

infixl 9 .|.

||| x = 0 (mod z) means z|x (z divides x).
|||
||| Idris steals too many operator spellings, though, so we spell the type with
||| periods.
(.|.) : (z, x : Nat) -> Type
z .|. x = ModularCongruence x 0 z

||| A type only inhabited by primes.
data Prime : Nat -> Type where
  ||| We can construct a prime x all strictly smaller z do not divide x.
  MkPrime : (x : Nat) -> ((z : Nat) -> (S z) `LTE` x -> Not (z .|. x)) -> Prime x

||| Our rules for output.
data OutputSemantics : Nat -> Output -> Type where
  OutputBuzz      : Fib n x -> 3 .|. x -> Not (5 .|. x) -> OutputSemantics n Buzz
  OutputFizz      : Fib n x -> 5 .|. x -> Not (3 .|. x) -> OutputSemantics n Fizz
  OutputFizzBuzz  : Fib n x -> 15 .|. x -> OutputSemantics n FizzBuzz
  OutputBuzzFizz  : Fib n x -> Prime n -> OutputSemantics n BuzzFizz
  OutputLiterally : Fib n x -> Not (3 .|. x) -> Not (5 .|. x) -> Not (Prime x) -> OutputSemantics n (Literally x)

||| Proof: There is only one possible output for any n.
outputIsFunctional : OutputSemantics n output1 -> OutputSemantics n output2 -> output1 = output2
outputIsFunctional (OutputBuzz _ _ _) (OutputBuzz _ _ _) = Refl
outputIsFunctional (OutputBuzz z w f) (OutputFizz y s g) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz z w f) (OutputFizz y s g) | Refl = absurd (f s)
outputIsFunctional (OutputBuzz z w f) (OutputFizzBuzz y s) = ?outputIsFunctional_rhs_8
outputIsFunctional (OutputBuzz z w f) (OutputBuzzFizz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz z w f) (OutputBuzzFizz y s) | Refl = ?outputIsFunctional_rhs_1
outputIsFunctional (OutputBuzz z w f) (OutputLiterally y g s t) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz z w f) (OutputLiterally y g s t) | Refl = absurd (g w)
outputIsFunctional (OutputFizz z w f) y = ?outputIsFunctional_rhs_2
outputIsFunctional (OutputFizzBuzz z w) y = ?outputIsFunctional_rhs_3
outputIsFunctional (OutputBuzzFizz z w) y = ?outputIsFunctional_rhs_4
outputIsFunctional (OutputLiterally z f g w) y = ?outputIsFunctional_rhs_5


