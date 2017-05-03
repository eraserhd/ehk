--
-- Exercises from Harry Garrood's "A guide to the PureScript numeric
-- hierarchy"
--

%default total

-- Exercise 2.3 - Uniqueness of identity elements

identityElementsAreUnique :
  (G : Type) ->
  (op : G -> G -> G) ->
  (p1 : (e1 : G ** (x : G) -> ((x `op` e1) = x, (e1 `op` x) = x))) ->
  (p2 : (e2 : G ** (x : G) -> ((x `op` e2) = x, (e2 `op` x) = x))) ->
  fst p1 = fst p2
identityElementsAreUnique _ op (e1 ** f1) (e2 ** f2) = trans b a
  where
    a : (e2 `op` e1) = e2
    a = fst $ f1 e2

    b : e1 = (e2 `op` e1)
    b = sym $ snd $ f2 e1
