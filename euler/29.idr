module Main

uniq : Eq a => List a -> List a
uniq [] = []
uniq [x] = [x]
uniq (x :: y :: zs) =
  if x == y
  then uniq (y :: zs)
  else x :: uniq (y :: zs)

distinctPowers : Nat
distinctPowers =
  length $ uniq $ sort [ pow a b | a <- [2..100], b <- [2..100] ]

main : IO ()
main =
  putStrLn $ show $ distinctPowers
