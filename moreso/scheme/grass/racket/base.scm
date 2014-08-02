;;;; base.scm - base library for Racket


(define (%current-process-id) 0)		;XXX use foreign interface
(define %void void)
(define %get-environment-variable getenv)
(define %current-error-port current-error-port)
(define %system system)
(define (%current-time) (current-seconds))
(define %current-directory current-directory)

(define (%file-exists? fname)
  (and (or (file-exists? fname)
	   (directory-exists? fname))
       fname))

(define (%open-input-file fn)
  (open-input-file fn #:exists 'replace #:mode 'binary))

(define (%open-output-file fn)
  (open-output-file fn #:exists 'replace #:mode 'binary))

(define %delete-file delete-file)
(define (%exit . status) (exit (if (pair? status) (car status) 0)))
(define %flush-output flush-output)

(define (%catch-system-errors h thunk)
  (with-handlers* 
   ((exn:break? 
     (lambda (ex)
       (h "interrupted" '())))
    (exn:fail? 
     (lambda (ex)
       (h (exn-message ex) '()))))	;XXX
   (thunk)))

(define %features '(racket-grass))	;XXX windows detection

(define %command-line-arguments
  (let ((args '()))
    (lambda as
      (if (pair? as)
	  (set! args (car as))
	  args))))

(define %call-with-exit-continuation call-with-current-continuation)
