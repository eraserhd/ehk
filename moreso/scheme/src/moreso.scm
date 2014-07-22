
;; Procedures

(define moreso:procedure-tag '#(moreso:procedure))

(define (moreso:lambda arg-list body lexical-environment)
  `#(,moreso:procedure-tag ,arg-list ,body ,lexical-environment))

(define (moreso:procedure? p)
  (and (vector? p)
       (eq? moreso:procedure-tag (vector-ref p 0))))

(define (moreso:procedure-arg-list p)
  (vector-ref p 1))

(define (moreso:procedure-body p)
  (vector-ref p 2))

(define (moreso:procedure-lexical-environment p)
  (vector-ref p 3))

(define (moreso:bind-args env args-passed args-specified)
  (cond
    ((and (null? args-passed)
	  (null? args-specified))
     env)

    ((symbol? args-specified)
     (cons (cons args-specified args-passed) env))

    ((null? args-specified)
     (raise "Too many parameters"))

    ((null? args-passed)
     (raise "Too few parameters"))

    (else
      (moreso:bind-args
	(cons (cons (car args-specified)
		    (car args-passed))
	      env)
	(cdr args-passed)
	(cdr args-specified)))))

(define (moreso:apply p args)
  (if (moreso:procedure? p)
    (let ((environment (moreso:bind-args
			 (moreso:procedure-lexical-environment p)
			 args
			 (moreso:procedure-arg-list p))))
      (moreso:eval (car (moreso:procedure-body p)) environment))
    (apply p args)))

(define (moreso:lookup sym env)
  (assq sym env))

(define (moreso:eval expr env)
  (cond
    ((symbol? expr)
     (let ((cell (moreso:lookup expr env)))
       (if cell
	 (cdr cell)
	 (raise (string-append "Unbound symbol `"
			       (symbol->string expr)
			       "'")))))

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
	      (set-cdr! cell (moreso:eval (caddr expr) env))
	      (raise (string-append "Unbound symbol `"
				    (symbol->string (cadr expr))
				    "'"))))))

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
