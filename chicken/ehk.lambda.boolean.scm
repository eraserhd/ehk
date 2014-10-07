(module ehk.lambda.boolean (TRUE FALSE IF)

  (import chicken scheme)
  (use ehk.lambda.parens)

  (define TRUE (parenthesize '(λt. λf. t)))
  (define FALSE (parenthesize '(λt. λf. f)))
  (define IF (parenthesize '(λc. λt. λf. c t f)))

  )
