;;;; main.scm - startup code for S48


(define library-path #f)
(define (set-library-path fn) (set! library-path fn))

(define (run-main args)
  (load-dynamic-externals (string-append library-path "/posix.so") #f #f #t)
  (main (map os-string->string args))
  0)
