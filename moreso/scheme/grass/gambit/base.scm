;;;; base.scm - base library for gambit


(c-declare "#include <unistd.h>")

(define %current-process-id
  (c-lambda () int "getpid"))

(define %list list)
(define %void (lambda _ (void)))

(define %get-environment-variable getenv)

(define %current-error-port current-error-port)
(define %system shell-command)
(define (%current-time) (time->seconds (current-time)))

(define (%file-exists? fname)
  (and (file-exists? fname) fname))

(define (%open-input-file fn)
  (open-input-file fn '(char-encoding: iso-8859-1 eol-encoding: lf)))

(define (%open-output-file fn)
  (open-output-file fn '(char-encoding: iso-8859-1 eol-encoding: lf)))

(define (%delete-file fn)
  (and (file-exists? fn)
       (let ((info (file-info fn)))
	 (case (file-info-type info)
	   ((directory) (delete-directory fn))
	   (else (delete-file fn))))))

(define %exit exit)
(define %flush-output force-output)

(define (%catch-system-errors h thunk)
  (with-exception-catcher
   (lambda (ex)
     ;; can an exception API be more braindead? I doubt it can.
     (cond ((error-exception? ex)
	    (h (error-exception-message ex)
	       (error-exception-parameters ex)))
	   ((heap-overflow-exception? ex)
	    (h "heap overflow" '()))
	   ((stack-overflow-exception? ex)
	    (h "stack overflow" '()))
	   ((os-exception? ex)
	    (h (os-exception-message ex)
	       (os-exception-arguments ex)))
	   ((no-such-file-or-directory-exception? ex)
	    (h "no such file or directory"
	       (no-such-file-or-directory-exception-arguments ex)))
	   ((unbound-os-environment-variable-exception? ex)
	    (h "unbound os environment variable"
	       (unbound-os-environment-variable-exception-arguments ex)))
	   ((type-exception? ex)
	    (h "type error"
	       (type-exception-arguments ex)))
	   ((range-exception? ex)
	    (h "range error"
	       (range-exception-arguments ex)))
	   ((divide-by-zero-exception? ex)
	    (h "divide by zero"
	       (divide-by-zero-exception-arguments ex)))
	   ((improper-length-list-exception? ex)
	    (h "improper length list"
	       (improper-length-list-exception-arguments ex)))
	   ((wrong-number-of-arguments-exception? ex)
	    (h "wrong number of arguments"
	       (wrong-number-of-arguments-exception-arguments ex)))
	   ((nonprocedure-operator-exception? ex)
	    (h "call of nonprocoedure"
	       (nonprocedure-operator-exception-operator ex)))
	   (else (h "unknown error" '()))))
   thunk))

(define %features '(gambit-grass))	;XXX windows detection

(define %command-line-arguments
  (let ((args '()))
    (lambda as
      (if (pair? as)
	  (set! args (car as))
	  args))))

(define %call-with-exit-continuation call-with-current-continuation)
