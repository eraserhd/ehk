(use matchable)

(test-group "E[M/x]"
  (test 42 (E<M/x> 42 79 'x))
  (test '42 (E<M/x> 'x 42 'x))
  (test '(/ 42 y) (E<M/x> '(/ x y) 42 'x))
  (test '(/ (/ y 42) 42) (E<M/x> '(/ (/ y x) x) 42 'x))
  (test '(\\ z (/ f f)) (E<M/x> '(\\ z (/ x x)) 'f 'x))
  (test '(\\ x (/ x x)) (E<M/x> '(\\ x (/ x x)) 'f 'x))
  (test '(\\ z (/ (/ + 1) (/ + 1))) (E<M/x> '(\\ z (/ x x)) '(/ + 1) 'x))
  (test-assert (match (E<M/x> '(\\ y (/ x y)) '(/ 1 (/ y y)) 'x)
		 [`(\\ ,(? symbol? q) (/ (/ 1 (/ y y)) ,q)) #t])))
