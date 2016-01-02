module Main
import Data.Fin
import Data.Vect

||| A recursive type to represent pan-digital numbers (permuations of 0-9).
||| It expands as (Fin n, Fin n-1, ..., Fin 1, ()), so the first element is
||| the first choice of digit, the second is the index of the digit from
||| the remaining set, and so forth.
PanDigitalNumber : (base : Nat) -> Type
PanDigitalNumber Z = ()
PanDigitalNumber (S n) = (Fin (S n), PanDigitalNumber n)

||| Find the first pandigital number for some PanDigitalNumber n
first : {n : Nat} -> PanDigitalNumber n
first {n=Z}    = ()
first {n=S n'} = (FZ, first)

||| Increment a pan-digital-number, if possible.  Returns Nothing if we
||| already have the highest number
next : {n : Nat} -> PanDigitalNumber n -> Maybe (PanDigitalNumber n)
next {n=Z}   _       = Nothing
next {n=S _} (d, ds) = case strengthen (FS d) of
                         Left _      => case next ds of
                                          Nothing     => Nothing
                                          Just nextDs => Just (FZ, nextDs)
                         Right nextD => Just (nextD, ds)

||| So this part was kind of annoying.  First I generated a list of all the
||| but that blew the stack (there are 10!).  I looked at Stream types, but
||| streams are infinite and have no Nil.  I tried to do lazy lists, but couldn't
||| get them to type-check because the head needs to be forced while the tail
||| delayed (I might be missing something here).
passing : {n : Nat} -> (PanDigitalNumber n -> Bool) -> List (PanDigitalNumber n)
passing {n=Z}   _  = []
passing {n=S n'} f =
  collect first []
  where
    collect : PanDigitalNumber (S n') -> List (PanDigitalNumber (S n')) -> List (PanDigitalNumber (S n'))
    collect pdn acc =
      let acc' = if f pdn
                 then pdn :: acc
                 else acc
      in 
        case next pdn of
          Just pdn' => collect pdn' acc'
          Nothing   => acc'

toDigits : {n : Nat} -> PanDigitalNumber n -> Vect n Int
toDigits {n=S x} pdn = digits pdn (map (toIntNat . finToNat) range)
  where
    digits : {n : Nat} -> PanDigitalNumber n -> Vect n Int -> Vect n Int
    digits {n=Z}   _       left = []
    digits {n=S _} (d, ds) left = let thisDigit = index d left in
                                  thisDigit :: digits ds (deleteAt d left)

toString : {n : Nat} -> PanDigitalNumber n -> String
toString pdn = pack $ map chr $ map (+ 48) $ toDigits pdn

toInteger : {n : Nat} -> PanDigitalNumber n -> Integer
toInteger pdn = foldl (\acc, x => acc * 10 + x) 0 (map cast $ toDigits pdn)

||| The tests are right out of the problem statement.
|||
||| on the case:  I can't add a test at the end which says
|||
|||   _ impossible
|||
||| because Idris thinks it's possible, in spite of the type being Vect 10 Int.
||| I wonder about this.  I'm not even sure what it thinks it could match so
||| I could write `void' cases. :/
ok : PanDigitalNumber 10 -> Bool
ok pdn = 
  case toDigits pdn of
    d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: d8 :: d9 :: d10 :: Nil =>
      (mod d4 2) == 0 &&
      (mod (d3 * 100 + d4 * 10 + d5) 3) == 0 &&
      (mod d6 5) == 0 &&
      (mod (d5 * 100 + d6 * 10 + d7) 7) == 0 &&
      (mod (d6 * 100 + d7 * 10 + d8) 11) == 0 &&
      (mod (d7 * 100 + d8 * 10 + d9) 13) == 0 &&
      (mod (d8 * 100 + d9 * 10 + d10) 17) == 0

main : IO ()
main = putStrLn $ show $ foldl (+) 0 $ map toInteger $ passing ok
