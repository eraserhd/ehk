--
-- Exercises from Harry Garrood's "A guide to the PureScript numeric
-- hierarchy"
--

%default total

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
