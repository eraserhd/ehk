
(define (moreso:lookup sym env)
  (assq sym env))

(define-structure moreso:procedure
  arg-list
  body-exprs
  lexical-env)

(define moreso:lambda make-moreso:procedure)

(define (moreso:apply p args)
  (if (moreso:procedure? p)
    (let ((procedure-env (let env-loop ((env (moreso:procedure-lexical-env p))
					(args-left args)
					(parameters-left (moreso:procedure-arg-list p)))
			   (cond
			     ((and (null? args-left)
				   (null? parameters-left))
			      env)

			     (else
			       (env-loop
				 (cons (cons (car parameters-left)
					     (car args-left))
				       env)
				 (cdr args-left)
				 (cdr parameters-left)))))))
      (moreso:eval
	(car (moreso:procedure-body-exprs p))
	procedure-env))
    (apply p args)))

(define (moreso:eval expr env)
  (cond
    ((symbol? expr)
     (let ((cell (moreso:lookup expr env)))
       (if cell
	 (cdr cell)
	 (raise (string-append "Unbound symbol `"
			       (symbol->string expr)
			       "'.")))))

    ((list? expr)
     (case (car expr)
       ;; Special forms
       ((if)
	(if (moreso:eval (cadr expr) env)
	  (moreso:eval (caddr expr) env)
	  (moreso:eval (cadddr expr) env)))

       ((quote)
	(if (= 2 (length expr))
	  (cadr expr)
	  (raise "`quote' expects a single form")))

       ((set!)
	(if (not (= 3 (length expr)))
	  (raise "`set!' expects two forms")
	  (let ((cell (moreso:lookup (cadr expr) env)))
	    (if cell
	      (set-cdr! cell (caddr expr))
	      (raise (string-append "Unbound symbol `"
				    (symbol->string expr)
				    "'."))))))

       (else
	 (let loop ((remaining-args expr)
		    (evaluated-args '()))
	   (if (null? remaining-args)
	     (let ((args (reverse evaluated-args)))
	       (moreso:apply (car args) (cdr args)))
	     (loop
	       (cdr remaining-args)
	       (cons (moreso:eval (car remaining-args) e) evaluated-args)))))))

    (else
     expr)))
