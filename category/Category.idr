

infixr 5 ~>
infixr 5 :=:

||| A fully general, but fully strict implementation of a category.
|||
|||   Obj   - the type of objects
|||   (~>) - the type constructor for morphisms
|||   (.)   - composition on morphisms
|||   (:=:) - A type constructor for equality on morphisms.  Note that we can't just use (=) because intensional equality
|||           doesn't work for morphisms in Set (f . g = p could never be shown).
|||   composeAssociative - Proof that composition is associative
|||   identity - constructor for identity morphisms, with a proof of identity properties
|||
data Category : Type where
  MkCategory : (Obj   : Type) ->
               ((~>)  : Obj -> Obj -> Type) ->
               ((.)   : {a, b, c : Obj} -> (a ~> b) -> (b ~> c) -> (a ~> c)) ->
               ((:=:) : {a, b, c, d : Obj} -> (f : a ~> b) -> (g : c ~> d) -> Type) ->
               (composeAssociative : {a, b, c, d : Obj} -> (f : a ~> b) -> (g : b ~> c) -> (h : c ~> d) -> (f . g) . h :=: f . (g . h)) ->
               (identity : (x : Obj) -> Subset (x ~> x) (\id => ((a : Obj) -> (f : x ~> a) -> id . f :=: f,
                                                                 (a : Obj) -> (g : a ~> x) -> g . id :=: g))) ->
               Category
