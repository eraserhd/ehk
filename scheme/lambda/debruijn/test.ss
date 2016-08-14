#!/usr/bin/env scheme --program

(import (chezscheme)
        (lambda debruijn))

(assert (equal? '(λ 1) (debruijnize '(λ x x) '())))
(assert (equal? '(λ ($ 1 1)) (debruijnize '(λ x ($ x x)) '())))
(assert (equal? '(λ (λ ($ 1 2))) (debruijnize '(λ x (λ y ($ y x))) '())))
