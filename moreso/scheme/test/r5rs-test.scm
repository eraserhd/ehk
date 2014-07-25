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
(expect (= 96 (moreso:eval '(cond (else 72 96)) moreso:r5rs)))

(expect (= 42 (moreso:eval '(case 7
			      ((6 8 9) 79)
			      ((5 7 10) 42)
			      ((11 13) #f)
			      (else #t)) moreso:r5rs)))
(expect (eq? moreso:unspecified (moreso:eval '(case 7 ((6) 42)) moreso:r5rs)))
(expect (= 79 (moreso:eval '(case 4 ((6) 42) (else 79)) moreso:r5rs)))

;; 4.2.3 Sequencing

(expect (= 42 (moreso:eval '(begin 41 42) moreso:r5rs)))
(expect (equal? moreso:unspecified (moreso:eval '(begin) moreso:r5rs)))
