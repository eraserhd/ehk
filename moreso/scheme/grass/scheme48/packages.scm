;;;; packages.scm - Scheme48 configuration for grass


(define-interface grass-base-interface
  (export %list
	  %void
	  %command-line-arguments
	  %current-error-port
	  %exit
	  %flush-output
	  %delete-file
	  %file-exists?
	  %system
	  %current-time
	  %get-environment-variable
	  %current-process-id
	  %catch-system-errors
	  %call-with-exit-continuation
	  %open-input-file
	  %open-output-file
	  %current-directory
	  %get-environment-variable
	  %features))

(define-structure grass-base grass-base-interface
  (open scheme 
	i/o
	srfi-9
	srfi-34
	conditions
	os-strings
	c-system-function
	posix)
  (files "base.scm"))

(define-structure grass (export main)
  (open scheme 
	i/o
	srfi-23
	c-system-function
	grass-base)
  (files "../build/core.scm"))

(define-structure moreso-main (export run-main set-library-path)
  (open scheme 
	os-strings
	load-dynamic-externals
	grass)
  (files "main.scm"))
