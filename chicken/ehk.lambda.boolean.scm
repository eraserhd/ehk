(module ehk.lambda.boolean (TRUE)

  (import chicken scheme)
  (use ehk.lambda.parens)

  (define TRUE (parenthesize '(λt. λf. t)))

  )
