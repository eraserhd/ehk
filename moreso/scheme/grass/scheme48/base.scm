;;;; base.scm - base library for Scheme48


(define (%current-process-id)
  (process-id->integer (get-process-id)))

(define %void (let ((u (if #f #f))) (lambda _ u)))
(define %list list)

(define (%get-environment-variable name)
  (let ((val (lookup-environment-variable name)))
    (and val
	 (os-string->string val))))

(define (%current-directory . arg)
  (if (pair? arg)
      (set-working-directory! (car arg))
      (os-string->string (working-directory))))

(define %current-error-port current-error-port)
(define %system system)
(define (%current-time) (time-seconds (current-time)))

(define (%file-exists? fname)
  (and (accessible? fname (access-mode exists))
       fname))

(define %delete-file unlink)

(define (%exit . status) 
  (exit (if (pair? status) (car status) 0)))

(define %flush-output force-output)

(define %open-input-file open-input-file)
(define %open-output-file open-output-file)

(define (%catch-system-errors h thunk)
  (with-exception-handler
   (lambda (ex)
     (h (if (condition? ex)
	    (condition-message ex)
	    "unknown error")
	'()))
   thunk))

(define %features '(scheme48-grass))	;XXX windows detection

(define %command-line-arguments
  (let ((args '()))
    (lambda as
      (if (pair? as)
	  (set! args (car as))
	  args))))

(define %call-with-exit-continuation call-with-current-continuation)
