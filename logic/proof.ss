;;   English       | formal logic | scheme syntax  
;;   ---------------------------------------------
;;   A             | A            | A              
;;   if A then B   | A ⇒ B        | (implies A B)  
;;   A or B        | A ∨ B        | (or A B)       
;;   A and B       | A ∧ B        | (and A B)      
;;   contradiction | ⊥            | _              
;;   not A         | ¬A           | (implies A _)
;; 
;; A proof is a list, where the car is a statement and the cdr is a list of
;; assumptions. The statement is true if the assumptions are true.
;;
;; For example: A ∴ (A ∨ B), alternately, A ⇒ A ∨ B looks like this: 
;; ((or A B) A)

(import (match))

(define-syntax fails?
  (syntax-rules ()
    ((fails? expr)
     (guard (error (else #t))
       expr
       #f))))

(define (assume proposition)
  (list proposition proposition))

(assert (equal? (assume 'C) '(C C)))

(define (proven! proof proposition)
  (assert (equal? proof (list proposition))))

(define (introduce-and left-proof right-proof)
  (cons (list 'and (car left-proof) (car right-proof))
        (append (cdr left-proof)
                (cdr right-proof))))

(assert (equal? (introduce-and '(A A) '(B B)) '((and A B) A B)))

(define (eliminate-and-left proof)
  (assert (eq? (caar proof) 'and))
  (cons (cadar proof) (cdr proof)))

(assert (equal? (eliminate-and-left '((and A B) C)) '(A C)))
(assert (equal? (eliminate-and-left '((and A B))) '(A)))
(assert (fails? (eliminate-and-left '((or A B) C))))

(define (and-E2 proof)
  (assert (eq? (caar proof) 'and))
  (cons (caddar proof) (cdr proof)))

(assert (equal? (and-E2 '((and A B) C)) '(B C)))
(assert (fails? (and-E2 '((or A B) C))))

(define (introduce-or-left left-proof right-proposition)
  (cons (list 'or (car left-proof) right-proposition)
        (cdr left-proof)))

(assert (equal? (introduce-or-left '(A A) 'B) '((or A B) A)))

(define (introduce-or-right left-proposition right-proof)
  (cons (list 'or left-proposition (car right-proof))
        (cdr right-proof)))

(assert (equal? (introduce-or-right 'A '(B B)) '((or A B) B)))

(define (eliminate-or or-proof left-proof right-proof)
  (assert (eq? (caar or-proof) 'or))
  (assert (equal? (car left-proof) (car right-proof)))
  (assert (member (cadar or-proof) (cdr left-proof)))
  (assert (member (caddar or-proof) (cdr right-proof)))
  (cons (car left-proof)
        (append
          (cdr or-proof)
          (remove (cadar or-proof) (cdr left-proof))
          (remove (caddar or-proof) (cdr right-proof)))))

(assert (equal? (eliminate-or '((or A B) X) '(C A Y) '(C B Z))
                '(C X Y Z)))
(assert (equal? (eliminate-or '((or A B) X) '(C A Y A) '(C B B Z))
                '(C X Y Z)))
(assert (fails? (eliminate-or '((or A B) X) '(C A Y) '(D B Z))))
(assert (fails? (eliminate-or '((and A B) X) '(C A Y) '(C B Z))))
(assert (fails? (eliminate-or '((or A B) X) '(C Y) '(C B Z))))
(assert (fails? (eliminate-or '((or A B) X) '(C A Y) '(C Z))))

(define (introduce-implication proof assumption)
  (cons (list 'implies assumption (car proof))
        (remove assumption (cdr proof))))

(assert (equal? (introduce-implication '(A B C) 'B) '((implies B A) C)))
(assert (equal? (introduce-implication '(A X Y) 'B) '((implies B A) X Y)))

(define (eliminate-implication left-proof implication-proof)
  (assert (equal? (car left-proof) (cadar implication-proof)))
  (cons (caddar implication-proof)
        (append
          (cdr left-proof)
          (cdr implication-proof))))

(assert (equal? (eliminate-implication '(B A) '((implies B C) X)) '(C A X)))
(assert (fails? (eliminate-implication '(G A) '((implies B C) X))))

(define (bottom-E bottom-proof proposition)
  (assert (eq? '_ (car bottom-proof)))
  (cons proposition (cdr bottom-proof)))

(assert (equal? (bottom-E '(_ X) 'A) '(A X)))
(assert (fails? (bottom-E '(Y X) 'A)))

;; Proof: (A ∨ ¬A) ⇒ (¬¬A ⇒ A)
(let* ((not-not-A '(implies (implies A _) _))
       (A-case (introduce-implication (assume 'A) not-not-A))
       (not-A-case (introduce-implication
                     (bottom-E
                       (eliminate-implication
                         (assume '(implies A _))
                         (assume not-not-A))
                       'A)
                     not-not-A))
       (EM '(or A (implies A _)))
       (both-cases (eliminate-or (assume EM) A-case not-A-case))
       (proof (introduce-implication both-cases EM)))
  (proven! proof '(implies (or A (implies A _)) (implies (implies (implies A _) _) A))))

;; vi:set sts=2 sw=2 ai et:
