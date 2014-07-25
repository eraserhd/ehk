;; Utilities

(define (reduce proc initial-value list)
  (cond
    ((null? list)
     initial-value)
    ((null? (cdr list)) ; last element is a tail call
     (proc initial-value (car list)))
    (else
      (reduce proc (proc initial-value (car list)) (cdr list)))))

;; Miscellaneous

(define (group-expressions expression-list)
  (if (= 1 (length expression-list))
    (car expression-list)
    (cons 'begin expression-list)))

(define moreso:unspecified '#(moreso:unspecified))

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
      (moreso:eval (moreso:procedure-body p) environment))
    (apply p args)))

(define (eval-begin expr env)
  (reduce
    (lambda (_ expr)
      (moreso:eval expr env))
    moreso:unspecified
    (cdr expr)))

(define (eval-if expr env)
  (if (moreso:eval (cadr expr) env)
    (moreso:eval (caddr expr) env)
    (if (= 4 (length expr))
      (moreso:eval (cadddr expr) env)
      moreso:unspecified)))

(define (eval-set! expr env)
  (if (not (= 3 (length expr)))
    (raise "`set!' expects two forms")
    (let ((cell (assq (cadr expr) env)))
      (if cell
	(set-cdr! cell (moreso:eval (caddr expr) env))
	(raise (string-append "Unbound symbol `"
			      (symbol->string (cadr expr))
			      "'"))))))

(define (eval-list expr env)
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
       ((begin)
	(eval-begin expr env))
       
       ((if)
	(eval-if expr env))

       ((lambda)
	(moreso:lambda (cadr expr) (group-expressions (cddr expr)) env))

       ((quote)
	(if (= 2 (length expr))
	  (cadr expr)
	  (raise "`quote' expects a single form")))

       ((set!)
	(eval-set! expr env))

       (else
	 (let ((first-form (moreso:eval (car expr) env))
	       (unevaluated-args (cdr expr)))
	   (if (moreso:macro? first-form)
	     (moreso:eval (moreso:apply (moreso:macro-procedure first-form)
					(cdr expr))
			  env)
	     (let ((evaluated-args (map (lambda (arg) (moreso:eval arg env)) unevaluated-args)))
	       (moreso:apply first-form evaluated-args)))))))

    (else
     expr)))

(define (eval-symbol sym env)
  (let ((cell (assq sym env)))
       (if cell
	 (cdr cell)
	 (raise (string-append "Unbound symbol `"
			       (symbol->string sym)
			       "'")))))

(define (moreso:eval expr env)
  (cond
    ((symbol? expr)
     (eval-symbol expr env))

    ((list? expr)
     (eval-list expr env))

    (else
     expr)))

;; Default environment

(define moreso:cond
  (moreso:make-macro
    (lambda args
      (let* ((reversed-conditions (reverse args))
	     (else-cond? (and (not (null? reversed-conditions))
			      (eq? 'else (caar reversed-conditions))))
	     (else-expr (if else-cond?
			  (cadar reversed-conditions)
			  moreso:unspecified))
	     (conds-left (if else-cond?
			   (cdr reversed-conditions)
			   reversed-conditions)))
	(reduce
	  (lambda (exprs a-cond)
	    `(if ,(car a-cond)
	       ,(group-expressions (cdr a-cond))
	       ,exprs))
	  else-expr
	  conds-left)))))

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
	     (binding-names (map car bindings))
	     (binding-exprs (map cadr bindings))
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
    (car . ,car)
    (cdr . ,cdr)
    (cond . ,moreso:cond)
    (let . ,moreso:let)
    (null? . ,null?)))
