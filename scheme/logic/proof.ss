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
          eliminate-contradiction)
  (import (rnrs)
          (utils match))

  (define-syntax match-define
    (syntax-rules ()
      ((_ name exprs ...)
       (define (name . args)
         (match args
           exprs ...)))))

  (define (assume proposition)
    (list proposition proposition))

  (define (therefore proof proposition)
    (assert (equal? proof (list proposition))))

  (define (introduce-and left-proof right-proof)
    (cons (list (car left-proof) 'and (car right-proof))
          (append (cdr left-proof)
                  (cdr right-proof))))

  (match-define eliminate-and-left
    [(((,A and ,B) ,assuming ...)) `(,A ,@assuming)])

  (match-define eliminate-and-right
    [(((,A and ,B) ,assuming ...)) `(,B ,@assuming)])

  (define (introduce-or-left left-proof right-proposition)
    (cons (list (car left-proof) 'or right-proposition)
          (cdr left-proof)))

  (define (introduce-or-right left-proposition right-proof)
    (cons (list left-proposition 'or (car right-proof))
          (cdr right-proof)))

  (match-define eliminate-or
    [(((,A or ,B) ,assuming-or ...)
      (,C ,assuming-left ...)
      (,C ,assuming-right ...))
     (guard (and (member A assuming-left)
                 (member B assuming-right)))
     `(,C ,@(append assuming-or
                    (remove A assuming-left)
                    (remove B assuming-right)))])

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

)

;; vi:set sts=2 sw=2 ai et:
