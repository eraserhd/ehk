(module ehk.lambda.boolean (TRUE FALSE)

  (import chicken scheme)
  (use ehk.lambda.parens)

  (define TRUE (parenthesize '(λt. λf. t)))
  (define FALSE (parenthesize '(λt. λf. f)))

  )
