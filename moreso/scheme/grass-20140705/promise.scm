;;;; promise.scm - implementation of delay/force
;
; syntax.scm dtype.scm


(define-values (make-promise-object %promise? %promise-thunk)
  (make-disjoint-type 'promise))

(define (%make-promise proc)
  (let ((result-ready #f)
	(results #f) )
    (make-promise-object
     (lambda ()
       (if result-ready
	   (apply values results)
	   (call-with-values 
	    proc
	    (lambda xs
	      (if result-ready
		  (apply values results)
		  (begin
		    (set! result-ready #t)
		    (set! results xs)
		    (apply values results) ) ) ) ) ) ) ) ) )

(define (%force x)
  (if (%promise? x)
      ((%promise-thunk x))
      x))
