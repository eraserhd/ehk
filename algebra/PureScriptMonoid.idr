--
-- Exercises from Harry Garrood's "A guide to the PureScript numeric
-- hierarchy"
--

%default total

data StrictMonoid : (M : Type) -> (op : M -> M -> M) -> Type where
  SM : (M : Type) ->
       (op : M -> M -> M) ->
       (assoc : ((a : M) -> (b : M) -> (c : M) ->
                 ((a `op` b) `op` c) = (a `op` (b `op` c)))) ->
       (ident : Exists (\e : M => (x : M) -> ((x `op` e) = x, (e `op` x) = x))) ->
       StrictMonoid M op

-- Exercise 2.1 - Show why (ℤ, -) is not a monoid

-- zMinusNotAMonoid : StrictMonoid Nat (Prelude.Nat.-) -> Void

-- Exercise 2.2 - Prove that (ℚ, +) is a monoid

data Q : Type where
  Ratio : Nat -> Nat -> Q

qPlus : Q -> Q -> Q
qPlus (Ratio ln ld) (Ratio rn rd) = Ratio (ln * rd + rn * ld) (rd * ld)

qPlusAssociative :
  (a : Q) -> (b : Q) -> (c : Q) ->
  ((a `qPlus` b) `qPlus` c) = (a `qPlus` (b `qPlus` c))
qPlusAssociative (Ratio an ad) (Ratio bn bd) (Ratio cn cd) =
  rewrite (multAssociative cd bd ad) in
  rewrite (multDistributesOverPlusLeft (an * bd) (bn * ad) cd) in
  rewrite (multDistributesOverPlusLeft (bn * cd) (cn * bd) ad) in
  rewrite (plusAssociative (an * (cd * bd)) ((bn * cd) * ad) ((cn * bd) * ad)) in
  rewrite (multAssociative cn bd ad) in
  rewrite (sym $ multAssociative an bd cd) in
  rewrite (sym $ multAssociative bn ad cd) in
  rewrite (sym $ multAssociative bn cd ad) in
  rewrite (multCommutative ad cd) in
  rewrite (multCommutative bd cd) in
  Refl

qPlusIdentity :
  Exists (\e : Q => (x : Q) -> ((x `qPlus` e) = x, (e `qPlus` x) = x))
qPlusIdentity = Evidence (Ratio 0 1) (\x => (rident x, lident x))
  where
    rident : (x : Q) -> (x `qPlus` (Ratio 0 1)) = x
    rident (Ratio n d) = rewrite (plusZeroRightNeutral d) in
                         rewrite (multOneRightNeutral n) in
                         rewrite (plusZeroRightNeutral n) in
                         Refl

    lident : (x : Q) -> ((Ratio 0 1) `qPlus` x) = x
    lident (Ratio n d) = rewrite (multOneRightNeutral n) in
                         rewrite (multOneRightNeutral d) in
                         Refl

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
