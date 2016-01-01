module Main
import Data.Fin
import Data.Vect

PanDigitalNumber : (base : Nat) -> Type
PanDigitalNumber Z = ()
PanDigitalNumber (S n) = (Fin (S n), PanDigitalNumber n)

first : {n : Nat} -> PanDigitalNumber n
first {n=Z}    = ()
first {n=S n'} = (FZ, first)

next : {n : Nat} -> PanDigitalNumber n -> Maybe (PanDigitalNumber n)
next {n=Z}   _       = Nothing
next {n=S _} (d, ds) = case strengthen (FS d) of
                         Left _      => case next ds of
                                          Nothing     => Nothing
                                          Just nextDs => Just (FZ, nextDs)
                         Right nextD => Just (nextD, ds)

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
