-- From the mIdris mailing list
import Data.Vect

%default total

-- Example from the mailing list.  How do you implement ?isElem_rhs
isElem' : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem' value []      = (No absurd)
isElem' value (x::xs) = case (decEq value x) of
                         Yes Refl   => Yes Here
                         No notHead => case isElem' value xs of
                                       Yes prf     => Yes (There prf)
                                       No notThere => No ?isElem_rhs

-- My solution
aux : DecEq ty => {value, x : ty} -> {xs : Vect n ty} -> ((value = x) -> Void) -> (Elem value xs -> Void) -> (Elem value (x :: xs) -> Void)
aux {value = x} {x = x} {xs = xs} notHead notTail Here = notHead Refl
aux {value = value} {x = x} {xs = xs} notHead notTail (There later) = notTail later

isElem2 : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem2 value []      = (No absurd)
isElem2 value (x::xs) with (decEq value x)
  isElem2 x (x::xs) | (Yes Refl) = Yes Here
  isElem2 value (x::xs) | (No notHead) with (isElem2 value xs)
    isElem2 value (x::xs) | (No notHead) | (Yes prf) = Yes (There prf)
    isElem2 value (x::xs) | (No notHead) | (No notTail) = No (aux notHead notTail)
