;;;; base.scm - base library for CHICKEN


;; Yuck! all just to avoid using "posix"

#+compiling
(begin
  (when (##sys#symbol-has-toplevel-binding? '##sys#signal-vector)
    (foreign-declare "#include <signal.h>")
    (let ((_sigint (foreign-value "SIGINT" int)))
      (##core#inline "C_establish_signal_handler" _sigint _sigint)
      (##sys#setslot ##sys#signal-vector _sigint 
		     (lambda (n) (##sys#user-interrupt-hook)))))
  (foreign-declare
   "#include <unistd.h>"
   "static char *get_current_directory() {"
   "static char getcwd_buffer[ PATH_MAX + 1 ];"
   "return getcwd(getcwd_buffer, PATH_MAX);}")

  (define %current-directory 
    ;; avoid requiring "posix" unit
    (let ((getcwd (foreign-lambda c-string "get_current_directory"))
	  (chdir (foreign-lambda int "chdir" c-string)))
      (lambda arg
	(if (pair? arg)
	    (or (zero? (chdir (car arg)))
		(error 'current-directory "can not change working directory" (car arg)))
	    (getcwd))))))


(case-sensitive #f)
(keyword-style #:none)

(define %list list)
(define %void void)
(define %current-error-port current-error-port)
(define %exit exit)
(define %flush-output flush-output)
(define %delete-file delete-file)
(define %file-exists? file-exists?)
(define %system system)
(define %current-time current-seconds)
(define %get-environment-variable get-environment-variable)
(define (%current-process-id) (foreign-value "getpid()" int))

(define (%open-input-file fn) (open-input-file fn #:binary))
(define (%open-output-file fn) (open-output-file fn #:binary))

(define user-interrupt? (condition-predicate 'user-interrupt))

(define (%catch-system-errors h thunk)
  (with-exception-handler
   (lambda (ex)
     (h (if (user-interrupt? ex)
	    "interrupted"
	    (get-condition-property ex 'exn 'message))
	(map (lambda (arg)
	       (if (procedure? arg)
		   (or (procedure-information arg) '<procedure>)
		   arg))
	     (or (get-condition-property ex 'exn 'arguments)
		 '()))))
   thunk))

(define %features '(chicken-grass))

(when (eq? 'mingw32 (build-platform))
  (set! %features (cons 'windows %features)))

(define %call-with-exit-continuation call-with-current-continuation)
(define %command-line-arguments command-line-arguments)
