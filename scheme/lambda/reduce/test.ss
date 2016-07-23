#!/usr/bin/env scheme --program

(import (scheme)
        (utils match)
        (lambda reduce))

(assert (equal? 42 (E<M/x> 42 79 'x)))
(assert (equal? '42 (E<M/x> 'x 42 'x)))

(assert (equal? '($ 42 y) (E<M/x> '($ x y) 42 'x)))
(assert (equal? '($ ($ y 42) 42) (E<M/x> '($ ($ y x) x) 42 'x)))
(assert (equal? '(λ z ($ f f)) (E<M/x> '(λ z ($ x x)) 'f 'x)))
(assert (equal? '(λ x ($ x x)) (E<M/x> '(λ x ($ x x)) 'f 'x)))
(assert (equal? '(λ z ($ ($ + 1) ($ + 1)))
                (E<M/x> '(λ z ($ x x)) '($ + 1) 'x)))
(assert (match (E<M/x> '(λ y ($ x y)) '($ 1 ($ y y)) 'x)
               [(λ ,q ($ ($ 1 ($ y y)) ,q))
                (guard (symbol? q))
                #t]))

(assert (equal? '(λ z ($ z z)) (α-convert '(λ x ($ x x)) 'z)))

(assert (equal? 42 (β-reduce '($ (λ x x) 42))))
(assert (equal? '($ (λ y ($ y 1)) (λ y ($ y 1)))
                (β-reduce '($ (λ x ($ x x)) (λ y ($ y 1))))))

(assert (equal? '($ + 1) (η-reduce '(λ x ($ ($ + 1) x)))))
(assert (equal? '(λ x ($ ($ + x) x)) (η-reduce '(λ x ($ ($ + x) x)))))

(assert (not (redex 42)))
(assert (eq? β-reduce (redex '($ (λ x 1) 2))))
(assert (eq? η-reduce (redex '(λ x ($ z x)))))

(assert (equal? `(,β-reduce) (normal-order '($ (λ x x) 42))))
(assert (equal? `(,β-reduce 1 1) (normal-order '($ ($ ($ (λ x x) 2) 5) 42))))
(assert (equal? `(,β-reduce 2) (normal-order '($ 4 ($ (λ x x) 42)))))

(assert (equal? 42 (reduce normal-order 42)))
(assert (equal? '(λ x x) (reduce normal-order '(λ x x))))
(assert (equal? 42 (reduce normal-order '($ (λ x x) 42))))
(assert (equal? 42 (reduce normal-order '($ ($ (λ x x) (λ y y)) 42))))
