#!/usr/bin/env scheme --program

(import (scheme)
	(lambda parens))

(assert (equal? '(λ x x) (parenthesize '(λx. x))))
(assert (equal? '(λ x 1) (parenthesize '(λx. 1))))
(assert (equal? '(λ y 4) (parenthesize '(λy. 4))))
(assert (equal? '(λ y (λ x 4)) (parenthesize '(λy. λx. 4))))
(assert (equal? '($ x y) (parenthesize '(x y))))
(assert (equal? '($ ($ x y) z) (parenthesize '(x y z))))
(assert (equal? '($ ($ ($ x y) z) w) (parenthesize '(x y z w))))
(assert (equal? '($ (λ x x) 4) (parenthesize '((λx. x) 4))))
(assert (equal? '($ f (λ x (λ y ($ ($ - y) x)))) (parenthesize '(f λx. λy. - y x))))

(assert (equal? '(λx. x) (deparenthesize '(λ x x))))
(assert (equal? '(λx. 1) (deparenthesize '(λ x 1))))
(assert (equal? '(λy. λx. 4) (deparenthesize '(λ y (λ x 4)))))
(assert (equal? '(x y) (deparenthesize '($ x y))))
(assert (equal? '(x y z) (deparenthesize '($ ($ x y) z))))
(assert (equal? '(x y z w) (deparenthesize '($ ($ ($ x y) z) w))))
(assert (equal? '(f λx. λy. - y x) (deparenthesize '($ f (λ x (λ y ($ ($ - y) x)))))))
(assert (equal? '((λx. x) 4) (deparenthesize '($ (λ x x) 4))))
(assert (equal? '((λx. x) λy. y) (deparenthesize '($ (λ x x) (λ y y)))))
