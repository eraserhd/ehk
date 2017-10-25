

infixr 5 ~>
infix 5 :=:

||| A fully general, but fully strict implementation of a category.
|||
|||   Obj   - the type of objects
|||
interface Category (Obj : Type) where
  ||| the type constructor for morphisms
  (~>) : Obj -> Obj -> Type

  ||| composition on morphisms
  (.) : {a, b, c : Obj} -> (a ~> b) -> (b ~> c) -> (a ~> c)

  ||| A type constructor for equality on morphisms.  Note that we can't just
  ||| use (=) because intensional equality doesn't work for morphisms in Set
  ||| (f . g = p could never be shown).
  (:=:) : {a, b : Obj} -> (a ~> b) -> (a ~> b) -> Type

  ||| Proof that composition is associative
  composeAssociative : {a, b, c, d : Obj} ->
                       (f : a ~> b) -> (g : b ~> c) -> (h : c ~> d) ->
                       (f . g) . h :=: f . (g . h)

  ||| Constructor for identity morphisms, with a proof of identity properties
  identity : (x : Obj) -> Subset (x ~> x) (\id => ((a : Obj) -> (f : x ~> a) -> id . f :=: f,
                                                   (a : Obj) -> (g : a ~> x) -> g . id :=: g))
