
(define (special-pythogorean-triplet)
  (call/cc
    (lambda (return)
      (for-each
	(lambda (a)
	  (for-each
	    (lambda (b)
	      (let ((c (- 1000 a b)))
		(if (= (+ (* a a) (* b b)) (* c c))
		  (return (list a b c)))))
	    (range from: 1 to: 1000)))
	(range from: 1 to: 1000)))))

(pp (apply * (special-pythogorean-triplet)))

