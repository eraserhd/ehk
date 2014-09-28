
(test-group "can distinguish binding terms"
  (test 'x (binding '\\x.))
  (test 'y (binding '\\y.))
  (test 'foo (binding '\\foo.))
  (test #f (binding 'x))
  (test #f (binding 42)))

(test-group "normalizing lambda abstractions"
  (test '(\\ x x) (normalize '(\\x. x)))
  (test '(\\ x 1) (normalize '(\\x. 1)))
  (test '(\\ y 4) (normalize '(\\y. 4)))
  (test '(\\ y (\\ x 4)) (normalize '(\\y. \\x. 4)))
  (test '(/ x y) (normalize '(x y)))
  (test '(/ (/ x y) z) (normalize '(x y z)))
  (test '(/ (/ (/ x y) z) w) (normalize '(x y z w)))
  (test '(/ (\\ x x) 4) (normalize '((\\x. x) 4)))
  (test '(/ f (\\ x (\\ y (/ (/ - y) x)))) (normalize '(f \\x. \\y. - y x))))
