;;;; fac.scm


(program
 (files "../lib/syntax.scm")
 (code
  (define (fac n)
    (if (zero? n)
	1
	(* n (fac (- n 1)))))
  (let ((x (fac 10)))
    (display x)
    (newline)
    (assert (= x 3628800)))))


