(library (lambda boolean)
  (export booleans)
  (import (rnrs)
          (lambda parens))

  (define (bind-all alist E)
    (fold-right
      (lambda (binding E)
	`($ (λ ,(car binding) ,E) ,(parenthesize (cdr binding))))
      E
      alist))

  (define (booleans E)
    (bind-all
      `((TRUE . (λt. λf. t))
	(FALSE . (λt. λf. f))
	(IF . (λc. λt. λf. c t f))
	(AND . (λa. λb. IF a b FALSE))
	(NOT . (λa. IF a FALSE TRUE))
	(OR . (λa. λb. IF a TRUE b)))
      E))

  )
