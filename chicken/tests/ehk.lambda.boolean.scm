
(test-group "boolean logic"
  (test "TRUE returns its first argument"
    42 (run '(TRUE 42 79)))
  (test "FALSE returns its second argument"
    79 (run '(FALSE 42 79)))
  (test "IF with TRUE returns its second argument"
    42 (run '(IF TRUE 42 79)))
  (test "IF with FALSE returns its third argument"
    79 (run '(IF FALSE 42 79)))
  (test (run 'TRUE) (run '(AND TRUE TRUE))))
