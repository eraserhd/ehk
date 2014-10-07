(module ehk.lambda.run (run)

  (import chicken scheme)
  (use ehk.lambda.reduce ehk.lambda.parens ehk.lambda.boolean)

  (define (run E)
    (reduce normal-order (booleans (parenthesize E))))

  )
