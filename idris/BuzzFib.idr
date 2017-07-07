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
-- TODO:
-- * Fix MkPrime
-- * Implement dividesTransitive

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

||| Only valid congurences (x = y (mod z)) are constructible.
|||
data ModularCongruence : (x, y, z : Nat) -> Type where
  ModularBase : (S n) `LTE` z -> ModularCongruence n n z
  ModularStep : ModularCongruence x' y' z' -> ModularCongruence (x' + z') y' z'

infixl 9 .|.

||| x = 0 (mod z) means z|x (z divides x).
|||
||| Idris steals too many operator spellings, though, so we spell the type with
||| periods.
(.|.) : (z, x : Nat) -> Type
z .|. x = ModularCongruence x 0 z

||| (.|.) is transitive
dividesTransitive : (x, y, z : Nat) -> x .|. y -> y .|. z -> x .|. z

||| A type only inhabited by primes.
data Prime : Nat -> Type where
  ||| We can construct a prime x all z such that z > 1 do not divide x.
  MkPrime : (x : Nat) -> ((z : Nat) -> 2 `LTE` z -> Not (z .|. x)) -> Prime x

||| Our rules for output.
data OutputSemantics : Nat -> Output -> Type where
  OutputBuzz      : Fib n x -> 3 .|. x -> Not (5 .|. x) -> OutputSemantics n Buzz
  OutputFizz      : Fib n x -> 5 .|. x -> Not (3 .|. x) -> OutputSemantics n Fizz
  OutputFizzBuzz  : Fib n x -> 15 .|. x -> OutputSemantics n FizzBuzz
  OutputBuzzFizz  : Fib n x -> Prime x -> OutputSemantics n BuzzFizz
  OutputLiterally : Fib n x -> Not (3 .|. x) -> Not (5 .|. x) -> Not (Prime x) -> OutputSemantics n (Literally x)

-- Some helpers
fiveDividesFifteen : ModularCongruence 15 0 5
fiveDividesFifteen = ModularStep $ ModularStep $ ModularStep $ ModularBase (LTESucc LTEZero)

threeDividesFifteen : ModularCongruence 15 0 3
threeDividesFifteen = ModularStep $ ModularStep $ ModularStep $ ModularStep $ ModularStep $ ModularBase (LTESucc LTEZero)

||| Proof: There is only one possible output for any n.
|||
||| There are 25 cases here (5 x 5), which is kind of yucky.  It could be cleaned
||| up by refactoring OutputSemantics; but I'd rather have the verbose proof with
||| the direct statement of requirements.
outputIsFunctional : OutputSemantics n output1 -> OutputSemantics n output2 -> output1 = output2
outputIsFunctional (OutputBuzz _ _ _) (OutputBuzz _ _ _) = Refl
outputIsFunctional (OutputFizz _ _ _) (OutputFizz _ _ _) = Refl
outputIsFunctional (OutputFizzBuzz _ _) (OutputFizzBuzz _ _) = Refl
outputIsFunctional (OutputBuzzFizz _ _) (OutputBuzzFizz _ _) = Refl
outputIsFunctional (OutputLiterally z f g w) (OutputLiterally y s t u) with (fibIsFunctional z y)
  outputIsFunctional (OutputLiterally z f g w) (OutputLiterally y s t u) | Refl = Refl

outputIsFunctional (OutputBuzz z w f) (OutputFizz y s g) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz z w f) (OutputFizz y s g) | Refl = absurd (f s)
outputIsFunctional (OutputBuzz {x} z w f) (OutputFizzBuzz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz {x} z w f) (OutputFizzBuzz y s) | Refl = absurd (f $ dividesTransitive 5 15 x fiveDividesFifteen s)
outputIsFunctional (OutputBuzz z w f) (OutputBuzzFizz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz z w f) (OutputBuzzFizz y (MkPrime x g)) | Refl = absurd (g 3 (LTESucc (LTESucc LTEZero)) w)
outputIsFunctional (OutputBuzz z w f) (OutputLiterally y g s t) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzz z w f) (OutputLiterally y g s t) | Refl = absurd (g w)

outputIsFunctional (OutputFizz z w f) (OutputBuzz y s g) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizz z w f) (OutputBuzz y s g) | Refl = absurd (f s)
outputIsFunctional (OutputFizz {x} z w f) (OutputFizzBuzz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizz {x} z w f) (OutputFizzBuzz y s) | Refl = absurd (f $ dividesTransitive 3 15 x threeDividesFifteen s)
outputIsFunctional (OutputFizz z w f) (OutputBuzzFizz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizz z w f) (OutputBuzzFizz y (MkPrime x g)) | Refl = absurd (g 5 (LTESucc (LTESucc LTEZero)) w)
outputIsFunctional (OutputFizz z w f) (OutputLiterally y g s t) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizz z w f) (OutputLiterally y g s t) | Refl = absurd (s w)

outputIsFunctional (OutputFizzBuzz {x} z w) (OutputBuzz y s f) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizzBuzz {x} z w) (OutputBuzz y s f) | Refl = absurd (f $ dividesTransitive 5 15 x fiveDividesFifteen w)
outputIsFunctional (OutputFizzBuzz {x} z w) (OutputFizz y s f) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizzBuzz {x} z w) (OutputFizz y s f) | Refl = absurd (f $ dividesTransitive 3 15 x threeDividesFifteen w)
outputIsFunctional (OutputFizzBuzz z w) (OutputBuzzFizz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizzBuzz z w) (OutputBuzzFizz y (MkPrime x f)) | Refl = absurd (f 15 (LTESucc (LTESucc LTEZero)) w)
outputIsFunctional (OutputFizzBuzz {x} z w) (OutputLiterally y f g s) with (fibIsFunctional z y)
  outputIsFunctional (OutputFizzBuzz {x} z w) (OutputLiterally y f g s) | Refl = absurd (g $ dividesTransitive 5 15 x fiveDividesFifteen w)

outputIsFunctional (OutputBuzzFizz z w) (OutputBuzz y s f) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzzFizz z (MkPrime x g)) (OutputBuzz y s f) | Refl = absurd (g 3 (LTESucc (LTESucc LTEZero)) s)
outputIsFunctional (OutputBuzzFizz z w) (OutputFizz y s f) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzzFizz z (MkPrime x g)) (OutputFizz y s f) | Refl = absurd (g 5 (LTESucc (LTESucc LTEZero)) s)
outputIsFunctional (OutputBuzzFizz z w) (OutputFizzBuzz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzzFizz z (MkPrime x f)) (OutputFizzBuzz y s) | Refl = absurd (f 15 (LTESucc (LTESucc LTEZero)) s)
outputIsFunctional (OutputBuzzFizz z w) (OutputLiterally y f g s) with (fibIsFunctional z y)
  outputIsFunctional (OutputBuzzFizz z w) (OutputLiterally y f g s) | Refl = absurd (s w)

outputIsFunctional (OutputLiterally z f g w) (OutputBuzz y s t) with (fibIsFunctional z y)
  outputIsFunctional (OutputLiterally z f g w) (OutputBuzz y s t) | Refl = absurd (f s)
outputIsFunctional (OutputLiterally z f g w) (OutputFizz y s t) with (fibIsFunctional z y)
  outputIsFunctional (OutputLiterally z f g w) (OutputFizz y s t) | Refl = absurd (g s)
outputIsFunctional (OutputLiterally {x} z f g w) (OutputFizzBuzz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputLiterally {x} z f g w) (OutputFizzBuzz y s) | Refl = absurd (g $ dividesTransitive 5 15 x fiveDividesFifteen s)
outputIsFunctional (OutputLiterally z f g w) (OutputBuzzFizz y s) with (fibIsFunctional z y)
  outputIsFunctional (OutputLiterally z f g w) (OutputBuzzFizz y s) | Refl = absurd (w s)

