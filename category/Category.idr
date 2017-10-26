

infixr 5 ~>
infix 5 :=:

||| Categories need to carry our idea of equality with them (for example,
||| in Set, we can't just use (=) because intensional equality doesn't work
||| for morphisms (f . g = p could never be shown).
interface Equality (eq : ty -> ty -> Type) where
  symmetry : (x, y : ty) -> x `eq` y -> y `eq` x
  reflexivity :  (x : ty) -> x `eq` x
  transitivity : (x, y, z : ty) -> x `eq` y -> y `eq` z -> x `eq` z

||| A fully general, but fully strict implementation of a category.
|||
|||   Obj   - the type of objects
|||
interface Category (Obj : Type) where
  ||| the type constructor for morphisms
  (~>) : Obj -> Obj -> Type

  ||| composition on morphisms
  (.) : {a, b, c : Obj} -> (a ~> b) -> (b ~> c) -> (a ~> c)

  (:=:) : {a, b : Obj} -> (a ~> b) -> (a ~> b) -> Type
  morphismEquality : Equality {ty=a ~> b} (:=:)

  ||| Proof that composition is associative
  composeAssociative : {a, b, c, d : Obj} ->
                       (f : a ~> b) -> (g : b ~> c) -> (h : c ~> d) ->
                       (f . g) . h :=: f . (g . h)

  ||| Constructor for identity morphisms, with a proof of identity properties
  identity : (x : Obj) -> Subset (x ~> x) (\id => ((a : Obj) -> (f : x ~> a) -> id . f :=: f,
                                                   (a : Obj) -> (g : a ~> x) -> g . id :=: g))
