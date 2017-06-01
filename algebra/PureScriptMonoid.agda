--
-- Exercises from Harry Garrood's "A guide to the PureScript numeric
-- hierarchy"
--
open import Data.Product
open import Data.Integer
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

data Monoid : (M : Set) → (_⊗_ : M → M → M) → Set where
  monoid : (M : Set) →
           (_⊗_ : M → M → M) →
           (⊗-associative : ∀ a b c → ((a ⊗ b) ⊗ c) ≡ (a ⊗ (b ⊗ c))) →
           (identity : ∃ (λ e → (∀ x → ((x ⊗ e) ≡ x) × ((e ⊗ x) ≡ x)))) →
           Monoid M _⊗_

-- Exercise 2.1 - Show why (ℤ, -) is not a monoid

ℤ-minus-not-monoid : ¬ Monoid ℤ _-_
ℤ-minus-not-monoid (monoid _ _ ⊗-associative _) with ⊗-associative (+ 1) (+ 1) (+ 1)
ℤ-minus-not-monoid (monoid _ _ _ _) | ()

-- Exercise 2.2 - Prove that (ℚ, +) is a monoid

data ℚ : Set where
  _÷_ : ℤ → ℤ → ℚ

_ℚ+_ : ℚ → ℚ → ℚ
(ln ÷ ld) ℚ+ (rn ÷ rd) = (ln * rd + rn * ld) ÷ (rd * ld)

ℚ+-associative : ∀ a b c → ((a ℚ+ b) ℚ+ c) ≡ (a ℚ+ (b ℚ+ c))
ℚ+-associative a b c = ?
