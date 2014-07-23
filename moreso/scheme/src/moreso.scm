
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
      (let body-loop ((body (moreso:procedure-body p))
		      (value (if #f #f)))
	(if (null? body)
	  value
	  (body-loop
	    (cdr body)
	    (moreso:eval (car body) environment)))))
    (apply p args)))

;; Macros

(define moreso:macro-tag '#(moreso:macro))

(define (moreso:make-macro procedure)
  (vector moreso:macro-tag procedure))

(define (moreso:macro? macro)
  (and (vector? macro)
       (eq? (vector-ref macro 0) moreso:macro-tag)))

(define (moreso:macro-procedure macro)
  (vector-ref macro 1))

;; Eval

(define (moreso:eval expr env)
  (cond
    ((symbol? expr)
     (let ((cell (assq expr env)))
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

       ((lambda)
	(moreso:lambda (cadr expr) (cddr expr) env))

       ((quote)
	(if (= 2 (length expr))
	  (cadr expr)
	  (raise "`quote' expects a single form")))

       ((set!)
	(if (not (= 3 (length expr)))
	  (raise "`set!' expects two forms")
	  (let ((cell (assq (cadr expr) env)))
	    (if cell
	      (set-cdr! cell (moreso:eval (caddr expr) env))
	      (raise (string-append "Unbound symbol `"
				    (symbol->string (cadr expr))
				    "'"))))))

       (else
	 (let ((first-form (moreso:eval (car expr) env))
	       (unevaluated-args (cdr expr)))
	   (if (moreso:macro? first-form)
	     (moreso:eval (moreso:apply (moreso:macro-procedure first-form)
					(cdr expr))
			  env)
	     (let ((evaluated-args (let loop ((evaluated '())
					      (remaining unevaluated-args))
				     (if (null? remaining)
				       (reverse evaluated)
				       (loop
					 (cons (moreso:eval (car remaining) env)
					       evaluated)
					 (cdr remaining))))))
	       (moreso:apply first-form evaluated-args)))))))

    (else
     expr)))

;; Default environment

(define (moreso:map proc list)
  (let loop ((remaining list)
	     (result '()))
    (if (null? remaining)
      (reverse result)
      (loop (cdr remaining) (cons (proc (car remaining)) result)))))

(define moreso:begin
  (moreso:make-macro
    (lambda args
      `(let () ,@args))))

(define moreso:cond
  (moreso:make-macro
    (lambda args
      `(if ,(caar args) (begin ,@(cdar args))))))

(define moreso:let
  (moreso:make-macro
    (lambda (bindings . body)
      (let* ((loop-label (if (symbol? bindings)
			   bindings
			   #f))
	     (bindings (if loop-label
			 (car body)
			 bindings))
	     (body (if loop-label
		     (cdr body)
		     body))
	     (binding-names (moreso:map car bindings))
	     (binding-exprs (moreso:map cadr bindings))
	     (binding-lambda `(lambda ,binding-names ,@body)))
	(if loop-label
	  `((lambda (,loop-label)
	      (set! ,loop-label ,binding-lambda)
	      (,loop-label ,@binding-exprs)) #f)
	  `(,binding-lambda ,@binding-exprs))))))

(define moreso:r5rs
  `((+ . ,+)
    (/ . ,/)
    (= . ,=)
    (begin . ,moreso:begin)
    (car . ,car)
    (cdr . ,cdr)
    (cond . ,moreso:cond)
    (let . ,moreso:let)
    (null? . ,null?)))