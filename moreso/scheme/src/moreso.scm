
(define (moreso:lookup sym env)
  (if (null? env)
    #f
    (let ((assq-result (assq sym (car env))))
      (if assq-result
	assq-result
	(moreso:lookup sym (cdr env))))))

(define (moreso:eval expr env)
  (cond
    ((symbol? expr)
     (let ((cell (moreso:lookup expr env)))
       (if cell
	 (cdr cell))))

    ((list? expr)
     (case (car expr)
       ;; Special forms
       ((if)
	(if (moreso:eval (cadr expr) env)
	  (moreso:eval (caddr expr) env)
	  (moreso:eval (cadddr expr) env)))))

    (else
     expr)))
