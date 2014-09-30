
(test-group "E[M/x] operation"
  (test 42 (E<M/x> 42 79 'x))
  (test '42 (E<M/x> 'x 42 'x))
  
  )
