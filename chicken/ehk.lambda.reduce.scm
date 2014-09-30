(module ehk.lambda.reduce (E<M/x>)

  (import chicken scheme)
  (use matchable)

  (define (E<M/x> E M x)
    (match E
      [('/ A B) `(/ ,(E<M/x> A M x) ,(E<M/x> B M x))]
      [('\\ y F) (if (eq? x y)
		   E
		   `(\\ ,y ,(E<M/x> F M x)))]
      [y (if (eq? x y) M y)]))

  )
