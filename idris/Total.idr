
{-
total
life : Nat -> Nat
life Z = 42
life k = life (pred k)
-}

{-
total
life : Nat -> Nat
life Z     = 42
life (S k) = life k
-}

{-
total
life : Nat -> Nat
life k = case k of
           Z => 42
           (S j) => life j
-}

{- Nope
total
life : Nat -> Nat
life n1 = case n1 of
            n2 => case n2 of
                    Z      => 42
                    (S n3) => case n3 of
                                Z      => life n3
                                (S n4) => life n4
-}

{- Nope
total
life : Nat -> Nat
life Z      = 42
life (S n1) = case n1 of
                Z      => life n1
                (S n2) => life n2
-}

{-
-- Works
total
life : Nat -> Nat
life (S (S x)) = life x
life _ = 42

-- Works
total
life : Nat -> Nat
life k = case k of
           (S (S y)) => life y
           _         => 42

-- Main.life is possibly not total due to recursive path Main.life --> Main.life
total
life : Nat -> Nat
life (S x) = case x of
               (S y) => life y
               _     => 42
life _ = 42
-}

total
life : Nat -> Nat -> Nat
life _ Z     = 42
life a (S b) = life b a
