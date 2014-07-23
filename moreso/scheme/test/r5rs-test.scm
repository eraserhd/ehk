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
                               42))
                           moreso:r5rs)))

;; 4.2.3 Sequencing

(expect (= 42 (moreso:eval '(begin 41 42) moreso:r5rs)))
(expect (equal? (if #f #f) (moreso:eval '(begin) moreso:r5rs)))
