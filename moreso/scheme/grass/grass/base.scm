;;; base.scm - base library for self-interpretation


(define %void void)
(define %command-line-arguments command-line-arguments)
(define %current-error-port current-error-port)
(define %exit exit)
(define %flush-output flush-output)
(define %delete-file delete-file)
(define %file-exists? file-exists?)
(define %catch-system-errors with-exception-handler)
(define %system system)
(define %open-input-file open-input-file)
(define %open-output-file open-output-file)
(define %current-time current-time)
(define %get-environment-variable get-environment-variable)
(define %current-directory current-directory)

(define %features 
  (cons 'grass-grass (if (memq 'windows (features)) '(windows) '())))

(define %current-process-id current-process-id)
(define %call-with-exit-continuation call-with-current-continuation)
