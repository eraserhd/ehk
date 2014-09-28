
(test-group "can distinguish binding terms"
  (test 'x (binding '\\x.))
  (test 'y (binding '\\y.))
  (test 'foo (binding '\\foo.))
  (test #f (binding 'x))
  (test #f (binding 42)))

(test-group "normalizing simple lambda abstractions"
  (test '(\\ x x) (normalize '(\\x. x)))
  (test '(\\ x 1) (normalize '(\\x. 1)))
  (test '(\\ y 4) (normalize '(\\y. 4))))
