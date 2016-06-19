(use srfi-1)

(define (collatz-count n)
  (cond
    ((= 1 n) 1)
    ((even? n) (+ 1 (collatz-count (/ n 2))))
    (else (+ 1 (collatz-count (+ 1 (* n 3)))))))

(write (foldl
	 (lambda (acc n)
	   (let* ((cc (collatz-count n)))
	     (if (>= cc (car acc))
	       (cons cc n)
	       acc)))
	 (cons 1 1)
	 (iota 999999 1)))
