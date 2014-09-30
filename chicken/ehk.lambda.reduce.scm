(module ehk.lambda.reduce (E<M/x>)

  (import chicken scheme)
  (use matchable)

  (define (E<M/x> E M x)
    (match E
      [y (if (eq? x y) M y)]))

  )
