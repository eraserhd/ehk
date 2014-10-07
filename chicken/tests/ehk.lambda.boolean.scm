
(test-group "boolean logic"
  (test "TRUE returns its first argument"
    42 (reduce normal-order (booleans (parenthesize '(TRUE 42 79)))))
  (test "FALSE returns its second argument"
    79 (reduce normal-order (booleans (parenthesize '(FALSE 42 79)))))
  (test "IF with TRUE returns its second argument"
    42 (reduce normal-order (booleans (parenthesize '(IF TRUE 42 79)))))
  (test "IF with FALSE returns its third argument"
    79 (reduce normal-order (booleans (parenthesize '(IF FALSE 42 79))))))
