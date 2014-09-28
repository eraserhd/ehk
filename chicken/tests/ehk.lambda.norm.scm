
(test-group "can distinguish binding terms"
  (test 'x (binding '\\x.))
  (test 'y (binding '\\y.))
  (test 'foo (binding '\\foo.))
  (test #f (binding 'x))
  (test #f (binding 42)))

