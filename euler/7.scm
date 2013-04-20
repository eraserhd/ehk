
(define (prime? n)
  (= 1 (length (factors n))))

(define count 0)

(let loop ((n 2))
  (if (prime? n)
    (begin
      (set! count (+ count 1))
      (if (= 10001 count)
	(begin
	  (display n)
	  (newline))
	(loop (+ n 1))))
    (loop (+ n 1))))
