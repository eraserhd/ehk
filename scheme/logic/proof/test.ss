#!/usr/bin/env scheme --program

(import (chezscheme)
        (logic proof))

(define-syntax fails?
  (syntax-rules ()
    ((fails? expr)
     (guard (error (else #t))
       expr
       #f))))

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
  (therefore proof `(,excluded-middle => (,not-not-A => A))))



;; vi:set sts=2 sw=2 ai et:
