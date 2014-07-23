(include "expect.scm")
(include "../src/moreso.scm")

(expect (= 42 (moreso:eval '(let ((x 42)) x) moreso:r5rs)))
(expect (= 6 (moreso:eval '(let loop ((remaining '(1 2 3))
				      (result 0))
			     (if (null? remaining)
			       result
			       (loop (cdr remaining) (+ result (car remaining)))))
			  moreso:r5rs)))

;; 4.2.1 Conditionals

(expect (= 42 (moreso:eval '(cond
                              ((= 2 2)
                               42)
			      ((= 2 3)
			       7))
                           moreso:r5rs)))
(expect (equal? moreso:unspecified (moreso:eval '(cond ((= 2 3) 42)) moreso:r5rs)))
(expect (= 4 (moreso:eval '(cond
			     ((= 2 3) 42)
			     ((= 2 2) 4)) moreso:r5rs)))
(expect (= 72 (moreso:eval '(cond
			      (#f 42)
			      ((= 2 3) 98)
			      (else 72)) moreso:r5rs)))

;; 4.2.3 Sequencing

(expect (= 42 (moreso:eval '(begin 41 42) moreso:r5rs)))
(expect (equal? moreso:unspecified (moreso:eval '(begin) moreso:r5rs)))
