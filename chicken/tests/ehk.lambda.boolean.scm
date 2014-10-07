
(test "TRUE returns its first argument"
  42 (reduce normal-order `($ ($ ,TRUE 42) 79)))
(test "FALSE returns its second argument"
  79 (reduce normal-order `($ ($ ,FALSE 42) 79)))
