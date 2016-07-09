;;   English       | formal logic | scheme syntax  
;;   ---------------------------------------------
;;   A             | A            | A              
;;   if A then B   | A ⇒ B        | (A => B)  
;;   A or B        | A ∨ B        | (A or B)       
;;   A and B       | A ∧ B        | (A and B)      
;;   contradiction | ⊥            | _              
;;   not A         | ¬A           | (A => _)
;; 
;; A proof is a list, where the car is a statement and the cdr is a list of
;; assumptions. The statement is true if the assumptions are true.
;;
;; For example: A ∴ (A ∨ B), alternately, A ⇒ A ∨ B looks like this: 
;; ((A or B) A)

(library (logic proof)
  (export assume
          therefore
          introduce-and eliminate-and-left eliminate-and-right
          introduce-or-left introduce-or-right eliminate-or
          introduce-implication eliminate-implication
          eliminate-contradiction
          verify-logic-proof)
  (import (rnrs)
          (match))

  (define (assume proposition)
    (list proposition proposition))

  (define (therefore proof proposition)
    (assert (equal? proof (list proposition))))

  (define (introduce-and left-proof right-proof)
    (cons (list (car left-proof) 'and (car right-proof))
          (append (cdr left-proof)
                  (cdr right-proof))))

  (define (eliminate-and-left proof)
    (assert (eq? (cadar proof) 'and))
    (cons (caar proof) (cdr proof)))

  (define (eliminate-and-right proof)
    (assert (eq? (cadar proof) 'and))
    (cons (caddar proof) (cdr proof)))

  (define (introduce-or-left left-proof right-proposition)
    (cons (list (car left-proof) 'or right-proposition)
          (cdr left-proof)))

  (define (introduce-or-right left-proposition right-proof)
    (cons (list left-proposition 'or (car right-proof))
          (cdr right-proof)))

  (define (eliminate-or or-proof left-proof right-proof)
    (assert (eq? (cadar or-proof) 'or))
    (assert (equal? (car left-proof) (car right-proof)))
    (assert (member (caar or-proof) (cdr left-proof)))
    (assert (member (caddar or-proof) (cdr right-proof)))
    (cons (car left-proof)
          (append
            (cdr or-proof)
            (remove (caar or-proof) (cdr left-proof))
            (remove (caddar or-proof) (cdr right-proof)))))

  (define (introduce-implication proof assumption)
    (cons (list assumption '=> (car proof))
          (remove assumption (cdr proof))))

  (define (eliminate-implication left-proof implication-proof)
    (assert (eq? '=> (cadar implication-proof)))
    (assert (equal? (car left-proof) (caar implication-proof)))
    (cons (caddar implication-proof)
          (append
            (cdr left-proof)
            (cdr implication-proof))))

  (define (eliminate-contradiction bottom-proof proposition)
    (assert (eq? '_ (car bottom-proof)))
    (cons proposition (cdr bottom-proof)))

  ;; Tests

  (define-syntax fails?
    (syntax-rules ()
      ((fails? expr)
       (guard (error (else #t))
         expr
         #f))))

  (define (verify-logic-proof)
    (assert (equal? (assume 'C) '(C C)))

    (assert (equal? (introduce-and '(A A) '(B B)) '((A and B) A B)))

    (assert (equal? (eliminate-and-left '((A and B) C)) '(A C)))
    (assert (equal? (eliminate-and-left '((A and B))) '(A)))
    (assert (fails? (eliminate-and-left '((A or B) C))))

    (assert (equal? (eliminate-and-right '((A and B) C)) '(B C)))
    (assert (fails? (eliminate-and-right '((A or B) C))))

    (assert (equal? (introduce-or-left '(A A) 'B) '((A or B) A)))

    (assert (equal? (introduce-or-right 'A '(B B)) '((A or B) B)))

    (assert (equal? (eliminate-or '((A or B) X) '(C A Y) '(C B Z))
                    '(C X Y Z)))
    (assert (equal? (eliminate-or '((A or B) X) '(C A Y A) '(C B B Z))
                    '(C X Y Z)))
    (assert (fails? (eliminate-or '((A or B) X) '(C A Y) '(D B Z))))
    (assert (fails? (eliminate-or '((A and B) X) '(C A Y) '(C B Z))))
    (assert (fails? (eliminate-or '((A or B) X) '(C Y) '(C B Z))))
    (assert (fails? (eliminate-or '((A or B) X) '(C A Y) '(C Z))))

    (assert (equal? (introduce-implication '(A B C) 'B) '((B => A) C)))
    (assert (equal? (introduce-implication '(A X Y) 'B) '((B => A) X Y)))

    (assert (equal? (eliminate-implication '(B A) '((B => C) X)) '(C A X)))
    (assert (fails? (eliminate-implication '(G A) '((B => C) X))))
    (assert (fails? (eliminate-implication '(B A) '((B or C) X))))

    (assert (equal? (eliminate-contradiction '(_ X) 'A) '(A X)))
    (assert (fails? (eliminate-contradiction '(Y X) 'A)))

    ;; Proof: (A ∨ ¬A) ⇒ (¬¬A ⇒ A)
    (let* ((not-not-A '((A => _) => _))
           (A-case (introduce-implication (assume 'A) not-not-A))
           (not-A-case (introduce-implication
                         (eliminate-contradiction
                           (eliminate-implication
                             (assume '(A => _))
                             (assume not-not-A))
                           'A)
                         not-not-A))
           (excluded-middle '(A or (A => _)))
           (both-cases (eliminate-or (assume excluded-middle) A-case not-A-case))
           (proof (introduce-implication both-cases excluded-middle)))
      (therefore proof `(,excluded-middle => (,not-not-A => A)))))

)

;; vi:set sts=2 sw=2 ai et:
