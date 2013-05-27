
(define (has-1000-digits? n)
  (>= (string-length (number->string n)) 1000))

(let loop ((f (fibonacci))
           (n 1))
  (if (has-1000-digits? (car f))
    (begin
      (display n)
      (newline))
    (loop (cdr f) (+ n 1))))

;(display (detect has-1000-digits? (fibonacci)))
;(newline)
