
(test-group "can distinguish binding terms"
  (test #t (binding? '\\x.))
  (test #t (binding? '\\y.))
  (test #t (binding? '\\foo.))
  (test #f (binding? 'x))
  (test #f (binding? 42)))
