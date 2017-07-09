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
fibIsFunctional Fib0 Fib0 = Refl
fibIsFunctional Fib1 Fib1 = Refl
fibIsFunctional (FibN a1 b1) (FibN a2 b2) with (fibIsFunctional a1 a2)
  fibIsFunctional (FibN a1 b1) (FibN a2 b2) | Refl with (fibIsFunctional b1 b2)
    fibIsFunctional (FibN a1 b1) (FibN a2 b2) | Refl | Refl = Refl

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

-- Syntax considered harmful.  But this one works for now and makes things clearer.
syntax [a] "≡" [b] "⟦" mod [n] "⟧" = ModularCongruence a b n

||| Only valid congurences (a ≡ b ⟦mod n⟧) are constructible.
|||
data ModularCongruence : (a, b, n : Nat) -> Type where
  ||| Construct a congruence, supplying k
  |||
  ||| We can usually deduce the other things.
  |||
  ||| @ k          The k that solves (a - b = k * n)
  ||| @ subtractOk Proof that b ≤ a
  ||| @ bLTn       Proof that b < n
  ||| @ prf        Proof that a - b = k * n
  MkModularCongruence : {a, b, n : Nat} ->
                        (k : Nat) ->
                        {auto subtractOk : b `LTE` a} ->
                        {auto bLTn : (S b) `LTE` n} ->
                        {auto prf : a - b = k * n} ->
                        a ≡ b ⟦mod n⟧

infixl 9 .|.

||| x = 0 (mod z) means z|x (z divides x).
|||
||| Idris steals too many operator spellings, though, so we spell the type with
||| periods.
(.|.) : (z, x : Nat) -> Type
z .|. x = x ≡ 0 ⟦mod z⟧

||| (.|.) is transitive
dividesTransitive : (x, y, z : Nat) -> x .|. y -> y .|. z -> x .|. z

||| A type only inhabited by primes.
data Prime : Nat -> Type where
  ||| This reads, roughly, p is prime if every divisor is either 1 or p itself.
  MkPrime : {p : Nat} ->
            ((divisor : Nat) -> (divisor .|. p) -> Either (divisor = 1) (divisor = p)) ->
            Prime p

||| Our rules for output.
data OutputSemantics : Nat -> Output -> Type where
  OutputBuzz      : Fib n x -> Not (Prime x) -> 3 .|. x -> Not (5 .|. x) -> OutputSemantics n Buzz
  OutputFizz      : Fib n x -> Not (Prime x) -> 5 .|. x -> Not (3 .|. x) -> OutputSemantics n Fizz
  OutputFizzBuzz  : Fib n x -> 15 .|. x -> OutputSemantics n FizzBuzz
  OutputBuzzFizz  : Fib n x -> Prime x -> OutputSemantics n BuzzFizz
  OutputLiterally : Fib n x -> Not (3 .|. x) -> Not (5 .|. x) -> Not (Prime x) -> OutputSemantics n (Literally x)

-- Some helpers
fiveDividesFifteen : 5 .|. 15
fiveDividesFifteen = MkModularCongruence 3

threeDividesFifteen : 3 .|. 15
threeDividesFifteen = MkModularCongruence 5

||| Proof: There is only one possible output for any n.
|||
||| There are 25 cases here (5 x 5), which is kind of yucky.  It could be cleaned
||| up by refactoring OutputSemantics; but I'd rather have the verbose proof with
||| the direct statement of requirements.
outputIsFunctional : OutputSemantics n output1 -> OutputSemantics n output2 -> output1 = output2
outputIsFunctional x y = ?outputIsFunctional_rhs

