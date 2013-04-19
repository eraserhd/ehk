
(define (square x)
  (* x x))

(display (- (square (apply + (range from: 1 to: 100)))
	    (apply + (map square (range from: 1 to: 100)))))
(newline)

