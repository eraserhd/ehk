;;;; write.scm
;
; misc.scm


(define (%write-hook x port) #f)

(define (%output-to-port x rd . port)
  (let ((port (optional port %output-port)))
    (define wr (if rd write display))
    (define (out x) (wr x port))
    (define (out1 x) (display x port))
    (let show ((x x))
      (cond ((%write-hook x port))
	    ((vector? x)
	     (let ((len (vector-length x)))
	       (out1 "#")
	       (show (vector->list x))))
	    ((pair? x)
	     (out1 "(")
	     (show (car x))
	     (let loop ((x (cdr x)))
	       (cond ((null? x) (out1 ")"))
		     ((pair? x) 
		      (out1 " ")
		      (show (car x))
		      (loop (cdr x)))
		     (else 
		      (out1 " . ")
		      (show x)
		      (out1 ")")))))
	    (else (out x))))
    (%void)))

(define (%display x . port)
  (%output-to-port x #f (optional port %output-port)))

(define (%write x . port)
  ;; does not escape special symbols (like ".", ...)
  (%output-to-port x #t (optional port %output-port)))
