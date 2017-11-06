
> import Data.Vect

> %default total

> data Flower = Red | Yellow | Blue

First, we have n and a Vect n Flower.  These are givens:

> gardenProof : (n : Nat) -> (garden : Vect n Flower) ->

I happen to have the book here, and I think it's important that "all colors
are represented".  I think we actually only need that blue is represented
because we have claims for Red and Yellow:

>               Elem Blue garden ->

Picking three distinct flowers from a garden is really something like "forall
3-prefixes of a permutation of n flowers".  It could be done with something like
https://github.com/eraserhd/ehk/blob/master/euler/43.idr#L7-L13

I'm going to say "given three distinct indices" is easier:

>               (x, y, z : Fin n) -> Not (x = y) -> Not (x = z) -> Not (y = z) ->

Now we can model the claim of the first observer:

>               let picked = index x garden :: index y garden :: index z garden :: [] in
>               Elem Red picked ->

This could alternately be modeled as `Either ((index x garden) = Red) (Either ((index y garden) = Red) ((index z garden) = Red))`, which could make it easier to prove.

And the second:

>               Elem Yellow picked ->

And the conclusion:

>               Elem Blue picked

How to prove this?  There would probably be a bunch of lemmas (or auxiliary functions).
Still off the cuff, I would try to first prove there are only three flowers, from which
the conclusion looks like it would come easily.
