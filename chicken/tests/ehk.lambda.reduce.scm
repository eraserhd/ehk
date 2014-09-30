
(test-group "E[M/x] operation"
  (test 42 (E<M/x> 42 79 'x))
  (test '42 (E<M/x> 'x 42 'x))
  (test '(/ 42 y) (E<M/x> '(/ x y) 42 'x))
  (test '(/ (/ y 42) 42) (E<M/x> '(/ (/ y x) x) 42 'x))
  )
