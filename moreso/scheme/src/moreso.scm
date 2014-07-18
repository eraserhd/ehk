

(define (moreso:eval expr)
  (cond
    ((list? expr)
     (case (car expr)
       ;; Special forms
       ((if)
	(if (moreso:eval (cadr expr))
	  (moreso:eval (caddr expr))
	  (moreso:eval (cadddr expr))))))

    (else
     expr)))
