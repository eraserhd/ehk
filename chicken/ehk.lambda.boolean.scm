(module ehk.lambda.boolean (booleans)

  (import chicken scheme)
  (use matchable ehk.lambda.parens)

  (define (bind-all alist E)
    (match alist
      [() E]
      [((name . value) . rest)
       (bind-all rest `($ (λ ,name ,E) ,(parenthesize value)))]))

  (define (booleans E)
    (bind-all
      `((TRUE . (λt. λf. t))
	(FALSE . (λt. λf. f))
	(AND . (λa. λb. IF a b FALSE))
	(IF . (λc. λt. λf. c t f)))
      E))

  )
