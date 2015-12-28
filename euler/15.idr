module Main

latticePaths : Nat -> Nat
latticePaths n =
  div (div (fact (2 * n)) (fact n)) (fact n)

main : IO ()
main = putStrLn $ show $ latticePaths 20
