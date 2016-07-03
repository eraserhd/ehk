;;  A              ; A
;;  (implies A B)  ; A ⇒ B
;;  (or A B)       ; A ∨ B
;;  (and A B)      ; A ∧ B
;;  _              ; ⊥ 
;;
;; A proof is a list where the car is a statement and the cdr is a list of
;; assumptions.

(define-syntax fails?
  (syntax-rules ()
    ((fails? expr)
     (guard (error (else #t))
       expr
       #f))))

(define (and-I left-proof right-proof)
  (cons (list 'and (car left-proof) (car right-proof))
        (append (cdr left-proof)
                (cdr right-proof))))

(assert (equal? (and-I '(A A) '(B B)) '((and A B) A B)))

(define (and-E1 proof)
  (assert (eq? (caar proof) 'and))
  (cons (cadar proof) (cdr proof)))

(assert (equal? (and-E1 '((and A B) C)) '(A C)))
(assert (equal? (and-E1 '((and A B))) '(A)))
(assert (fails? (and-E1 '((or A B) C))))

(define (and-E2 proof)
  (assert (eq? (caar proof) 'and))
  (cons (caddar proof) (cdr proof)))

(assert (equal? (and-E2 '((and A B) C)) '(B C)))
(assert (fails? (and-E2 '((or A B) C))))

(define (or-I1 left-proof right-proposition)
  (cons (list 'or (car left-proof) right-proposition)
        (cdr left-proof)))

(assert (equal? (or-I1 '(A A) 'B) '((or A B) A)))

(define (or-I2 left-proposition right-proof)
  (cons (list 'or left-proposition (car right-proof))
        (cdr right-proof)))

(assert (equal? (or-I2 'A '(B B)) '((or A B) B)))

(define (or-E or-proof left-proof right-proof)
  (assert (eq? (caar or-proof) 'or))
  (assert (equal? (car left-proof) (car right-proof)))
  (assert (member (cadar or-proof) (cdr left-proof)))
  (assert (member (caddar or-proof) (cdr right-proof)))
  (cons (car left-proof)
        (append
          (cdr or-proof)
          (remove (cadar or-proof) (cdr left-proof))
          (remove (caddar or-proof) (cdr right-proof)))))

(assert (equal? (or-E '((or A B) X) '(C A Y) '(C B Z))
                '(C X Y Z)))
(assert (equal? (or-E '((or A B) X) '(C A Y A) '(C B B Z))
                '(C X Y Z)))
(assert (fails? (or-E '((or A B) X) '(C A Y) '(D B Z))))
(assert (fails? (or-E '((and A B) X) '(C A Y) '(C B Z))))
(assert (fails? (or-E '((or A B) X) '(C Y) '(C B Z))))
(assert (fails? (or-E '((or A B) X) '(C A Y) '(C Z))))

(define (implies-I proof assumption)
  (cons (list 'implies assumption (car proof))
        (remove assumption (cdr proof))))

(assert (equal? (implies-I '(A B C) 'B) '((implies B A) C)))
(assert (equal? (implies-I '(A X Y) 'B) '((implies B A) X Y)))

;; vi:set sts=2 sw=2 ai et:
