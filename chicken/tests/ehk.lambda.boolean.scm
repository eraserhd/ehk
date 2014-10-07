
(test "TRUE returns its first argument"
  42 (reduce normal-order `($ ($ ,TRUE 42) 79)))
