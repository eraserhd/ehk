
> import Data.Vect

> %default total

> data Flower = Red | Yellow | Blue

Let's first describe how to pick three flowers.

> data Pick3 : Vect n Flower -> Vect 3 Flower -> Type where
>   MkPick3 : (garden : Vect n Flower) ->
>             (x, y, z : Fin n) -> Not (x = y) -> Not (x = z) -> Not (y = z) ->
>             Pick3 garden (index x garden :: index y garden :: index z garden :: [])

First, we have n and a Vect n Flower.  These are givens:

> gardenProof : (n : Nat) -> (garden : Vect n Flower) ->

I happen to have the book here, and I think it's important that "all colors
are represented".  I think we actually only need that blue is represented
because we have claims for Red and Yellow:

>               Elem Blue garden ->

Now we can model the claim of the first observer:

>               (Pick3 garden picked -> Elem Red picked) ->

This could alternately be modeled as `Either ((index x garden) = Red) (Either ((index y garden) = Red) ((index z garden) = Red))`, which could make it easier to prove.

And the second:

>               (Pick3 garden picked -> Elem Yellow picked) ->

And the conclusion:

>               Pick3 garden picked -> Elem Blue picked

How to prove this?  There would probably be a bunch of lemmas (or auxiliary functions).
Still off the cuff, I would try to first prove there are only three flowers, from which
the conclusion looks like it would come easily.
