
(define sieve (make-vector 2000000 #t))

(let loop ((sum 2)
	   (p 3))
  (cond
    ((>= p 2000000)
     (display sum)
     (newline))

    ((vector-ref sieve p)
     (let s-loop ((n (+ p p)))
       (if (>= n 2000000)
	 #f
	 (begin
	   (vector-set! sieve n #f)
	   (s-loop (+ n p)))))
     (loop (+ sum p) (+ p 2)))

    (else
     (loop sum (+ p 2)))))
