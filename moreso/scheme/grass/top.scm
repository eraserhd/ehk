;;;; top.scm - command-line parsing and startup
;
; expand.scm eval.scm env.scm pp.scm write.scm dtype.scm match.scm


(add-feature 'grass)

(let ()
  (define (proc->string proc)
    (let ((name (%procedure-name proc)))
      (if name
	  (string-append "#<procedure " name ">")
	  "#<procedure>")))
  (set! %write-hook
    (lambda (x port)
      (cond ((%procedure? x)
	     (display (proc->string x) port)
	     #t)
	    ((%environment? x)
	     (display "#<environment " port)
	     (display (%environment-data-name (%environment-data x)) port)
	     (write-char #\> port))
	    ((%disjoint-type-instance? x) 
	     (display (disjoint-type-instance->string x) port)
	     #t)
	    (else #f))))
  (set! pretty-print-hook
    (lambda (x out wr)
      (cond ((%procedure? x)
	     (out (proc->string x)))
	    ((char-vector? x)
	     (wr (char-vector->string x)))
	    ((%environment? x)
	     (out "#<environment ")
	     (out (%environment-data-name (%environment-data x)))
	     (out ">"))
	    ((%disjoint-type-instance? x) 
	     (out (disjoint-type-instance->string x)))
	    (else #f)))))

(define (usage code)
  (display "usage: scheme [-h] [-FEATURE ...] [FILENAME ARGUMENT ...]\n" %error-port)
  (%exit code))

(define home (or (%get-environment-variable "HOME") "."))

(define *temporary-directory*
  (or (%get-environment-variable "TMP")
      (%get-environment-variable "TMPDIR")
      (%get-environment-variable "TEMP")
      "/tmp"))

(define (main args)
  (define (load-init)
    (cond ((%file-exists?
	    (string-append home "/.scheme/init.scm"))
	   => (lambda (f)
		;(emit "; loading " f " ...\n")
		(%load f)))))
  (define (eval-file in)
    (fluid-let ((*current-source-filename* in))
      (match (read-forms in %read)
	(('begin forms ...)
	 (for-each %eval forms)))))
  (define (eval-strings args)
    (let ((tmp (string-append
		*temporary-directory*
		"/scheme-startup."
		(number->string (%current-process-id))
		".scm")))
      (%with-output-to-file 
       tmp
       (lambda ()
	 (display "(begin " %output-port)
	 (for-each 
	  (lambda (str) 
	    (display str %output-port)
	    (write-char #\space %output-port))
	  args)
	 (display "\n)\n" %output-port)))
      (%load tmp)
      (%delete-file tmp)))
  (%initialize-ports 
   (current-input-port)
   (current-output-port)
   (%current-error-port))
  (%command-line-arguments args)
  (let loop ((args args))
    (match args
      (()
       (%display "[0m[42m[30mGRASS No. ")
       (%display *grass-version*)
       (%display "[0m\n")
       (add-feature 'interactive)
       (load-init)
       (%repl)
       (%exit))
      (((or "-h" "-help" "--help") . _) (usage 0))
      ((x . more)
       (let ((len (string-length x)))
	 (cond ((and (>= len 2)
		     (char=? #\- (string-ref x 0)))
		(add-feature (string->symbol (substring x 1 len)))
		(loop more))
	       ((and (positive? len) 
		     (char=? #\( (string-ref x 0)))
		(eval-strings args)
		(%exit))
	       (else
		(%command-line-arguments more)
		(eval-file x)
		(%exit))))))))
