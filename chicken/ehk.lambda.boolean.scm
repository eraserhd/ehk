(module ehk.lambda.boolean (booleans)

  (import chicken scheme)
  (use ehk.lambda.parens)

  (define TRUE (parenthesize '(λt. λf. t)))
  (define FALSE (parenthesize '(λt. λf. f)))
  (define IF (parenthesize '(λc. λt. λf. c t f)))

  (define (booleans E)
    `($ (λ IF ($ (λ FALSE ($ (λ TRUE ,E) ,TRUE)) ,FALSE)) ,IF))

  )
