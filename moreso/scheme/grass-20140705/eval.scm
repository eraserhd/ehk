;;;; eval.scm - standard evaluator
;
; syntax.scm misc.scm evalcore.scm env.scm pp.scm read.scm


(define compile #f)
(define set-toplevel-variable! #f)
(define %oblist #f)
(define *unbound* (list '*unbound*))
(define *uninitialized* (list '*uninitialized*))
(define *unbound-variables* #f)
(define *toplevel-environment* #f)

(define (abort-continuation) (%exit 1))

(define report-error
  (let ((pp pp))
    (lambda (msg args)
      (let ((out %error-port))
	(%flush-output %output-port)
	(display "\nError: " out)
	(display msg out)
	(when (pair? args)
	  (newline out)
	  (for-each
	   (lambda (x)
	     (newline out)
	     (pp x out))
	   args))
	(newline out)
	(%flush-output out)))))

(define (error-handler msg args)
  (report-error msg args)
  (abort-continuation))

(define (%error msg . args) (error-handler msg args))

(set! expand-error-hook %error)

(define (%with-exception-handler handler thunk)
  (let ((handler 
	 (lambda (msg args)
	   (handler msg args)
	   (display "\nError: exception-handler returned - terminating.\n" %error-port)
	   (%exit 1))))
    (fluid-let ((error-handler handler))
      (%catch-system-errors handler thunk))))


(let ()

  (define-syntax compiled-lambda
    (syntax-rules ()	
      ((_ llist info body ...)
       (lambda llist body ...))))

  (define-syntax compile-procedure-call
    (syntax-rules ()	
      ((_ form env name here tail default) default)))

  (define-syntax compile-form
    (syntax-rules ()	
      ((_ form env name here tail default) default)))

  (define-syntax compile-procedure-check
    (syntax-rules ()
      ((_ opexp default) default)))

  (define (potentially-unbound a set)
    (when (and *unbound-variables* (not set))
      (set! *unbound-variables* (cons a *unbound-variables*))))

  (define (lookup sym env set k) ; (lookup SYMBOL LOCALENV SET? K) -> (K FOUND? REF SET)
    (let ((oblist (%environment-data-oblist *toplevel-environment*)))
      (let findenv ((env env) (i 0))
	(if (null? env)
	    (cond ((assq sym oblist) =>
		   (lambda (a)
		     (k #f (lambda (e) (cdr a)) (lambda (e x) (set-cdr! a x)))))
		  (else
		   (let ((a (cons sym *unbound*)))
		     (potentially-unbound a set)
		     (when (%environment-data-mutable? *toplevel-environment*)
		       (%environment-data-oblist-set! *toplevel-environment* (cons a oblist)))
		     (k #f (lambda (e) (cdr a)) (lambda (e x) (set-cdr! a x))))))
	    (let findpos ((b (car env)) (j 0))
	      (cond ((null? b) (findenv (cdr env) (+ i 1)))
		    ((eq? sym (car b)) 
		     (k #t
			(lambda (e) 
			  (vector-ref (list-ref e i) j))
			(lambda (e x)
			  (vector-set! (list-ref e i) j x))))
		    (else (findpos (cdr b) (+ j 1)))))))))

  (define (fudge-argument-list n alst)
    (do ((n n (- n 1))
	 (c 0 (+ c 1))
	 (args alst 
	       (if (null? args)
		   (%error "bad argument count" n c)
		   (cdr args)))
	 (last #f args) )
	((= n 0)
	 (set-cdr! last (list args))
	 alst) ) )

  (define (proper-list? x)
    (cond ((null? x) #t)
	  ((pair? x) (proper-list? (cdr x)))
	  (else #f)))

  (define (check shape form)
    (define (fail)
      (%error "syntax error" form shape))
    (let walk ((x shape) (y form))
      (cond ((eq? x '_) form)
	    ((eq? x y) form)
	    ((pair? x)
	     (cond ((eq? '... (car x))
		    (if (proper-list? y)
			form
			(fail)))
		   ((pair? y)
		    (walk (car x) (car y))
		    (walk (cdr x) (cdr y))
		    form)
		   (else (fail))))
	    ((and (vector? x) (vector? y))
	     (walk (vector->list x) (vector->list y)))
	    (else (fail)))))

  (define (comp-call form env name here tail)
    (compile-procedure-call 
     form env name here tail
     (comp-call1 form env name here tail)))

  (define (comp-call1 form env name here tail)
    (check '(_ ...) form)
    (let* ((op (car form))
	   ;;XXX alexpander unfortunately doesn't let this through:
	   (op (if (%procedure? op) op (comp (car form) env #f here #f)))
	   (args (cdr form))
	   (argc (length args)))
      (define (checkf e)
	(let ((p (op e)))
	  (if (not (%procedure? p))
	      (%error "call of non-procedure" p))
	  (let ((arity (%procedure-arity p)))
	    (cond ((not arity))
		  ((= argc arity) (%procedure-code p))
		  ((negative? arity)
		   (let ((arity (- (abs arity) 1)))
		     (if (>= argc arity)
			 (%procedure-code p)
			 (%error 
			  (string-append
			   "procedure expected " 
			   (number->string arity)
			   " or more arguments but was called with "
			   (number->string argc))
			  p))))
		  (else
		   (%error
		    (string-append 
		     "procedure expected "
		     (number->string arity)
		     " argument" 
		     (if (= arity 1) "" "s")
		     " but was called with "
		     (number->string argc))
		    p))))))
      (case argc
	((0) (compiled-lambda
	      (e) (,here ,tail <call> ,form)
	      ((compile-procedure-check (op e) (checkf e)))))
	((1) (let ((a1 (comp (car args) env #f here #f)))
	       (compiled-lambda
		(e) (,here ,tail <call> ,form)
		((compile-procedure-check (op e) (checkf e)) (a1 e)))))
	((2) (let ((a1 (comp (car args) env #f here #f))
		   (a2 (comp (cadr args) env #f here #f)))
	       (compiled-lambda 
		(e) (,here ,tail <call> ,form)
		((compile-procedure-check (op e) (checkf e)) (a1 e) (a2 e)))))
	((3) (let ((a1 (comp (car args) env #f here #f))
		   (a2 (comp (cadr args) env #f here #f))
		   (a3 (comp (caddr args) env #f here #f)))
	       (compiled-lambda
		(e) (,here ,tail <call> ,form)
		((compile-procedure-check (op e) (checkf e)) (a1 e) (a2 e) (a3 e)))))
	((4) (let ((a1 (comp (car args) env #f here #f))
		   (a2 (comp (cadr args) env #f here #f))
		   (a3 (comp (caddr args) env #f here #f))
		   (a4 (comp (cadddr args) env #f here #f)))
	       (compiled-lambda
		(e) (,here ,tail <call> ,form)
		((compile-procedure-check (op e) (checkf e)) (a1 e) (a2 e) (a3 e) (a4 e)))))
	(else
	 (let ((as (map (lambda (a) (comp a env #f here #f)) args)))
	   (compiled-lambda
	    (e) (,here ,tail <call> ,form)
	    (let ((fn (compile-procedure-check (op e) (checkf e))))
	      (apply fn (map (lambda (a) (a e)) as)))))))))
  
  (define (comp form env name here tail)
    (compile-form
     form env name here tail
     (cond ((or (number? form) (string? form) (char? form) (boolean? form))
	    (compiled-lambda (e) (,here ,tail literal ,form) form))
	   ((symbol? form)
	    (lookup
	     form env #f
	     (lambda (found ref set)
	       (compiled-lambda 
		(e) (,here ,tail <variable> ,form)
		(let ((x (ref e)))
		  (if found
		      (if (eq? *uninitialized* x)
			  (%error "reference to uninitialized \"letrec\" variable" form)
			  x)
		      (if (eq? *unbound* x)
			  (%error "unbound variable" form)
			  x)))))))
	   ((pair? form)
	    (let ((op (car form))
		  (args (cdr form)))
	      (cond ((symbol? op)
		     (case op
		       ;; only handles forms that alexpander considers primitive
		       ((begin)
			(check '(begin ...) form)
			(let ((n (length args)))
			  (case n
			    ((0) (compiled-lambda (e) (,here ,tail begin ,form) (%void)))
			    ((1) (comp (car args) env name here tail))
			    ((2) (let* ((x1 (comp (car args) env #f here #f))
					(x2 (comp (cadr args) env name here tail)))
				   (compiled-lambda (e) (,here ,tail begin ,form) (x1 e) (x2 e))))
			    ((3) (let* ((x1 (comp (car args) env #f here #f))
					(x2 (comp (cadr args) env #f here #f))
					(x3 (comp (caddr args) env name here tail)))
				   (compiled-lambda (e) (,here ,tail begin ,form) (x1 e) (x2 e) (x3 e))))
			    ((4) (let* ((x1 (comp (car args) env #f here #f))
					(x2 (comp (cadr args) env #f here #f))
					(x3 (comp (caddr args) env #f here #f))
					(x4 (comp (cadddr args) env name here tail)))
				   (compiled-lambda (e) (,here ,tail begin ,form) (x1 e) (x2 e) (x3 e) (x4 e))))
			    (else
			     (let* ((x1 (comp (car args) env #f here #f))
				    (x2 (comp
					 `(begin ,@(cdr args))
					 env name here tail)))
			       (compiled-lambda (e) (,here ,tail begin ,form) (x1 e) (x2 e)))))))
		       ((quote)
			(check '(quote _) form)
			(let ((c (car args)))
			  (compiled-lambda (e) (,here ,tail quote ,form) c)))
		       ((if)
			(check '(if _ _ ...) form)
			;;XXX doesn't check for more than 3 arguments
			(let* ((x1 (comp (car args) env #f here #f))
			       (x2 (comp (cadr args) env name here tail))
			       (x3 (if (pair? (cddr args))
				       (comp (caddr args) env name here tail)
				       (compiled-lambda
					(e) (,here ,tail void (%void))
					(%void)))))
			  (compiled-lambda
			   (e) (,here ,tail if ,form)
			   (if (x1 e) (x2 e) (x3 e)))))
		       ((set! define)
			(check `(,(car form) _ _) form)
			(let* ((var (car args))
			       (val (comp (cadr args) env var here #f)))
			  (if (%environment-data-mutable? *toplevel-environment*)
			      (lookup
			       var env #t
			       (lambda (found ref set)
				 (compiled-lambda 
				  (e) (,here ,tail set! ,form)
				  (set e (val e)) (%void))))
			      (let ((name (%environment-data-name *toplevel-environment*)))
				(compiled-lambda
				 (e) (,here ,tail set! ,form)
				 (%error "environment is not mutable" name))))))
		       ((let)
			(check `(let _ _ ...) form)
			(let* ((bindings (car args))
			       (n (length bindings)))
			  ;;XXX check binding syntax
			  (if (zero? n)
			      (comp `(begin ,@(cdr args)) env name here tail)
			      (let* ((vars (map car bindings))
				     (env2 (cons vars env))
				     (vals 
				      (map (lambda (b)
					     (comp (cadr b) env (car b) here #f))
					   bindings))
				     (body (comp
					    `(begin ,@(cdr args)) 
					    env2 name here tail)))
				(compiled-lambda
				 (e) (,here ,tail let form)
				 (let* ((v (make-vector n (%void)))
					(e2 (cons v e)))
				   (do ((i 0 (+ i 1))
					(vals vals (cdr vals)))
				       ((>= i n))
				     (vector-set! v i ((car vals) e2)))
				   (body e2)))))))
		       ((letrec)
			(check `(letrec  _ _ ...) form)
			(let* ((bindings (car args))
			       (n (length bindings)))
			  ;;XXX check binding syntax
			  (cond ((zero? n)
				 (comp `(begin ,@(cdr args)) env name here tail))
				(else
				 (let* ((vars (map car bindings))
					(env2 (cons vars env))
					(vals 
					 (map (lambda (b)
						(comp (cadr b) env2 (car b) here #f))
					      bindings))
					(body (comp `(begin ,@(cdr args)) env2 name here tail)))
				   (compiled-lambda
				    (e) (,here ,tail letrec form)
				    (let* ((v (make-vector n *uninitialized*))
					   (tv (make-vector n))
					   (e2 (cons v e)))
				      (do ((i 0 (+ i 1))
					   (vals vals (cdr vals)))
					  ((>= i n))
					(vector-set! tv i ((car vals) e2)))
				      (do ((i 0 (+ i 1)))
					  ((>= i n))
					(vector-set! v i (vector-ref tv i)))
				      (body e2))))))))
		       ((lambda)
			(check '(lambda _ _ ...) form)
			(let ((llist (car args)))
			  (define (comp-lambda vars argc rest)
			    (if (null? vars)
				(let ((body (comp
					     `(begin ,@(cdr args)) 
					     env #f name #t)))
				  (compiled-lambda 
				   (e) (,here ,tail lambda ,form)
				   (%procedure
				    (compiled-lambda 
				     () (,name #f entry ,llist ,env ,e)
				     (body e))
				    name 0)))
				(let* ((env2 (cons vars env))
				       (body (comp
					      `(begin ,@(cdr args)) 
					      env2 #f name #t)))
				  (case argc
				    ((0) (compiled-lambda
					  (e) (,here ,tail lambda ,form)
					  ;; must have rest or would have handled above
					  (%procedure
					   (compiled-lambda
					    r (,name #f entry ,llist)
					    (body (cons (vector r) e)))
					   name -1)))
				    ((1) (if rest
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 . r) (,name #f entry ,llist)
						(body (cons (vector a1 r) e)))
					       name -2))
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1) (,name #f entry ,llist)
						(body (cons (vector a1) e)))
					       name 1))))
				    ((2) (if rest
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 a2 . r) (,name #f entry ,llist)
						(body (cons (vector a1 a2 r) e)))
					       name -3))
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 a2) (,name #f entry ,llist)
						(body (cons (vector a1 a2) e)))
					       name 2))))
				    ((3) (if rest
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 a2 a3 . r) 
						(,name #f entry ,llist)
						(body (cons (vector a1 a2 a3 r) e)))
					       name -4))
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 a2 a3) (,name #f entry ,llist)
						(body (cons (vector a1 a2 a3) e)))
					       name 3))))
				    ((4) (if rest
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 a2 a3 a4 . r)
						(,name #f entry ,llist)
						(body (cons (vector a1 a2 a3 a4 r) e)))
					       name -5))
					     (compiled-lambda
					      (e) (,here ,tail lambda ,form)
					      (%procedure
					       (compiled-lambda
						(a1 a2 a3 a4) 
						(,name #f entry ,llist)
						(body (cons (vector a1 a2 a3 a4) e)))
					       name 4))))
				    (else
				     (if rest
					 (compiled-lambda
					  (e) (,here ,tail lambda ,form)
					  (%procedure
					   (compiled-lambda 
					    as (,name #f entry ,llist)
					    (body
					     (cons
					      (apply 
					       vector
					       (fudge-argument-list argc as)) e)))
					   name (- (+ argc 1))))
					 (compiled-lambda
					  (e) (,here ,tail lambda ,form)
					  (%procedure
					   (compiled-lambda
					    as (,name #f entry ,llist)
					    (body (cons (list->vector as) e)))
					   name argc))))))))
			  (let loop ((ll llist) (vars '()) (argc 0))
			    (cond ((null? ll)
				   (comp-lambda (reverse vars) argc #f))
				  ((symbol? ll)
				   (comp-lambda (reverse (cons ll vars)) argc ll))
				  ((pair? ll)
				   (if (symbol? (car ll))
				       (loop (cdr ll)
					     (cons (car ll) vars)
					     (+ argc 1))
				       (%error "invalid lambda-list syntax" llist)))
				  (else (%error "invalid lambda-list syntax" llist))))))
		       (else (comp-call form env name here tail))))
		    (else (comp-call form env name here tail)))))
	   (else (%error "invalid form" form)))))
  
  (set! compile
    (lambda (form env)		; compiler
      (let ((data (%environment-data env)))
	(fluid-let ((*toplevel-environment* data))
	  (comp form '() #f '<toplevel> #t)))))
  (set! set-toplevel-variable!		; mutator
    (lambda (name val env)
      (let* ((data (%environment-data env))
	     (oblist (%environment-data-oblist data)))
	(cond ((assq name oblist) =>
	       (lambda (a)
		 (set-cdr! a val)))
	      (else
	       (vector-set!
		data 1
		(cons (cons name val) oblist)))))))
  (set! %oblist 
    (lambda env 
      (let* ((env (optional env %interaction-environment))
	     (data (%environment-data env)))
	(append
	 (let loop ((lst (car (%environment-data-mstore data))))
	   (match lst
	     (() (%environment-data-oblist data))
	     ((((? number?) . _) . more) (loop more))
	     ((x . more) (cons x (loop more))))))))))

(define %eval
  (let ((compile compile))
    (lambda (x . env)
      (let ((env (optional env %interaction-environment)))
	((compile (%expand x env) env) '())))))

(define *current-source-filename* #f)

(define (%load filename . evaluator)
  (let ((eval (optional evaluator %eval))
	(in (open-input-file filename))
	(ac abort-continuation))
    (fluid-let ((*current-source-filename* filename)
		(abort-continuation
		 (lambda ()
		   (close-input-port in)
		   (ac))))
      (let loop ()
	(let ((x (%read in)))
	  (if (not (eof-object? x))
	      (begin
		(eval x)
		(loop)))))
      (close-input-port in))))

(define *repl-level* 0)
(define (repl-exit result) (%exit))

(define (prompt level)
  (display (string-append (make-string level #\>) " ") %output-port)
  (%flush-output %output-port))

(define %repl-prompt prompt)

(define (%quit . result)
  (repl-exit (optional result (%void))))

(define (%repl . evaluator)
  (%call-with-exit-continuation
   (lambda (quit)
     (fluid-let ((*repl-level* (+ *repl-level* 1))
		 (repl-exit quit)
		 (*unbound-variables* '()))
       (define (report-unbound)
	 (let loop ((vars *unbound-variables*) (ub '()))
	   (match vars
	     (() 
	      (when (pair? ub)
		(let ((out %error-port))
		  (display "Warning: the following global variables are currently unbound:\n\n" out)
		  (for-each
		   (lambda (a)
		     (display "  " out)
		     (display (car a) out)
		     (newline out))
		   ub)
		  (newline out))))
	     (((and a (_ . val)) . more)
	      (loop more (if (eq? *unbound* val) (cons a ub) ub))))))
       (let* ((in %input-port)
	      (out %output-port)
	      (err %error-port)
	      (eval (optional evaluator %eval)))
	 (when (%environment-data-mutable? (%environment-data %interaction-environment))
	   (set-toplevel-variable! 'it (%void) %interaction-environment))
	 (%call-with-exit-continuation
	  (lambda (return)
	    (let loop ()
	      (let ((prompt-returned #f))
		(%call-with-exit-continuation
		 (lambda (k)
		   (fluid-let ((%input-port in)
			       (%output-port out)
			       (%error-port err)
			       (abort-continuation (lambda () (k #f))))
		     (set! *unbound-variables* '())
		     (%repl-prompt *repl-level*)
		     (set! prompt-returned #t)
		     (%with-exception-handler
		      (lambda (msg args)
			(report-error msg args)
			(k #f))
		      (lambda ()
			(let ((x (%read)))
			  (cond ((eof-object? x) (return x))
				(else
				 (when (eqv? (%peek-char) #\newline) (%read-char))
				 (call-with-values
				     (cut eval x)
				   (lambda rs
				     (report-unbound)
				     (match rs
				       (((? (cut eq? <> (%void)))) #f)
				       (() #f)
				       ((r1 . rs)
					(when (%environment-data-mutable? (%environment-data %interaction-environment))
					  (set-toplevel-variable! 'it r1 %interaction-environment))
					(for-each pp (cons r1 rs))
					(%flush-output %output-port)))))))))))))
		(unless prompt-returned
		  (display
		   "Error in \"repl-prompt\" procedure - restoring default prompt\n"
		   %error-port)
		  (set! %repl-prompt prompt)))
	      (loop))))
	 (newline out))))))
