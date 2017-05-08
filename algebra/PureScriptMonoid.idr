--
-- Exercises from Harry Garrood's "A guide to the PureScript numeric
-- hierarchy"
--
import Data.ZZ

%default total

data StrictMonoid : (M : Type) -> (op : M -> M -> M) -> Type where
  SM : (M : Type) ->
       (op : M -> M -> M) ->
       (assoc : ((a : M) -> (b : M) -> (c : M) ->
                 ((a `op` b) `op` c) = (a `op` (b `op` c)))) ->
       (ident : Exists (\e : M => (x : M) -> ((x `op` e) = x, (e `op` x) = x))) ->
       StrictMonoid M op

-- Exercise 2.1 - Show why (ℤ, -) is not a monoid

zMinusNotAMonoid : StrictMonoid ZZ (-) -> Void
zMinusNotAMonoid (SM _ _ assoc ident) = posNotNeg $ sym contradiction
  where
    contradiction : (the ZZ (-1)) = 1
    contradiction = assoc 1 1 1

-- Exercise 2.2 - Prove that (ℚ, +) is a monoid

-- FIXME: Prevent the possibility of the denominator being zero
-- FIXME: Numerator should always be positive (Nat)

data Q : Type where
  Ratio : ZZ -> ZZ -> Q

qPlus : Q -> Q -> Q
qPlus (Ratio ln ld) (Ratio rn rd) = Ratio (ln * rd + rn * ld) (rd * ld)

qPlusAssociative :
  (a : Q) -> (b : Q) -> (c : Q) ->
  ((a `qPlus` b) `qPlus` c) = (a `qPlus` (b `qPlus` c))
qPlusAssociative (Ratio an ad) (Ratio bn bd) (Ratio cn cd) =
  rewrite (multAssociativeZ cd bd ad) in
  rewrite (multDistributesOverPlusLeftZ (an * bd) (bn * ad) cd) in
  rewrite (multDistributesOverPlusLeftZ (bn * cd) (cn * bd) ad) in
  rewrite (plusAssociativeZ (an * (cd * bd)) ((bn * cd) * ad) ((cn * bd) * ad)) in
  rewrite (multAssociativeZ cn bd ad) in
  rewrite (sym $ multAssociativeZ an bd cd) in
  rewrite (sym $ multAssociativeZ bn ad cd) in
  rewrite (sym $ multAssociativeZ bn cd ad) in
  rewrite (multCommutativeZ ad cd) in
  rewrite (multCommutativeZ bd cd) in
  Refl

qPlusIdentity :
  Exists (\e : Q => (x : Q) -> ((x `qPlus` e) = x, (e `qPlus` x) = x))
qPlusIdentity = Evidence (Ratio 0 1) (\x => (rident x, lident x))
  where
    rident : (x : Q) -> (x `qPlus` (Ratio 0 1)) = x
    rident (Ratio n d) = rewrite (multOneRightNeutralZ n) in
                         rewrite (multOneLeftNeutralZ d) in
                         rewrite (multZeroLeftZeroZ d) in
                         rewrite (plusZeroRightNeutralZ n) in
                         Refl

    lident : (x : Q) -> ((Ratio 0 1) `qPlus` x) = x
    lident (Ratio n d) = rewrite (multOneRightNeutralZ n) in
                         rewrite (multOneRightNeutralZ d) in
                         rewrite (multZeroLeftZeroZ d) in
                         rewrite (plusZeroLeftNeutralZ n) in
                         Refl

qPlusMonoid : StrictMonoid Q (Main.qPlus)
qPlusMonoid = SM Q Main.qPlus qPlusAssociative qPlusIdentity

-- Exercise 2.3 - Uniqueness of identity elements

identityElementsAreUnique :
  (G : Type) ->
  (op : G -> G -> G) ->
  (p1 : Exists (\e : G => (x : G) -> ((x `op` e) = x, (e `op` x) = x))) ->
  (p2 : Exists (\e : G => (x : G) -> ((x `op` e) = x, (e `op` x) = x))) ->
  getWitness p1 = getWitness p2
identityElementsAreUnique _ op (Evidence e1 pf1) (Evidence e2 pf2) = trans b a
  where
    a : (e2 `op` e1) = e2
    a = fst $ pf1 e2

    b : e1 = (e2 `op` e1)
    b = sym $ snd $ pf2 e1
