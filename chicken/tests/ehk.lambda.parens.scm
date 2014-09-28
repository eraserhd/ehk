
(test-group "can distinguish binding terms"
  (test 'x (binding '\\x.))
  (test 'y (binding '\\y.))
  (test 'foo (binding '\\foo.))
  (test #f (binding 'x))
  (test #f (binding 42)))

(test-group "normalizing lambda abstractions"
  (test '(\\ x x) (parenthesize '(\\x. x)))
  (test '(\\ x 1) (parenthesize '(\\x. 1)))
  (test '(\\ y 4) (parenthesize '(\\y. 4)))
  (test '(\\ y (\\ x 4)) (parenthesize '(\\y. \\x. 4)))
  (test '(/ x y) (parenthesize '(x y)))
  (test '(/ (/ x y) z) (parenthesize '(x y z)))
  (test '(/ (/ (/ x y) z) w) (parenthesize '(x y z w)))
  (test '(/ (\\ x x) 4) (parenthesize '((\\x. x) 4)))
  (test '(/ f (\\ x (\\ y (/ (/ - y) x)))) (parenthesize '(f \\x. \\y. - y x))))

(test-group "denormalizing"
  (test '(\\x. x) (deparenthesize '(\\ x x)))
  (test '(\\x. 1) (deparenthesize '(\\ x 1)))
  (test '(\\y. \\x. 4) (deparenthesize '(\\ y (\\ x 4))))
  (test '(x y) (deparenthesize '(/ x y)))
  (test '(x y z) (deparenthesize '(/ (/ x y) z)))
  (test '(x y z w) (deparenthesize '(/ (/ (/ x y) z) w)))
  (test '(f \\x. \\y. - y x) (deparenthesize '(/ f (\\ x (\\ y (/ (/ - y) x))))))
  (test '((\\x. x) 4) (deparenthesize '(/ (\\ x x) 4)))
  (test '((\\x. x) \\y. y) (deparenthesize '(/ (\\ x x) (\\ y y)))))
