(use matchable)

(test-group "E[M/x]"
  (test 42 (E<M/x> 42 79 'x))
  (test '42 (E<M/x> 'x 42 'x))
  (test '($ 42 y) (E<M/x> '($ x y) 42 'x))
  (test '($ ($ y 42) 42) (E<M/x> '($ ($ y x) x) 42 'x))
  (test '(λ z ($ f f)) (E<M/x> '(λ z ($ x x)) 'f 'x))
  (test '(λ x ($ x x)) (E<M/x> '(λ x ($ x x)) 'f 'x))
  (test '(λ z ($ ($ + 1) ($ + 1))) (E<M/x> '(λ z ($ x x)) '($ + 1) 'x))
  (test-assert (match (E<M/x> '(λ y ($ x y)) '($ 1 ($ y y)) 'x)
		 [`(λ ,(? symbol? q) ($ ($ 1 ($ y y)) ,q)) #t])))

(test-group "α-convert"
  (test '(λ z ($ z z)) (α-convert '(λ x ($ x x)) 'z)))

(test-group "β-reduction"
  (test 42 (β-reduce '($ (λ x x) 42)))
  (test '($ (λ y ($ y 1)) (λ y ($ y 1))) (β-reduce '($ (λ x ($ x x)) (λ y ($ y 1))))))

(test-group "η-reduction"
  (test '($ + 1) (η-reduce '(λ x ($ ($ + 1) x))))
  (test '(λ x ($ ($ + x) x)) (η-reduce '(λ x ($ ($ + x) x)))))

(test-group "redex"
  (test-assert (not (redex 42)))
  (test β-reduce (redex '($ (λ x 1) 2)))
  (test η-reduce (redex '(λ x ($ z x)))))

(test-group "normal order"
  (test `(,β-reduce) (normal-order '($ (λ x x) 42)))
  (test `(,β-reduce 1 1) (normal-order '($ ($ ($ (λ x x) 2) 5) 42)))
  (test `(,β-reduce 2) (normal-order '($ 4 ($ (λ x x) 42)))))

(test-group "old/normal-order reduction"
  (test 42 (reduce old/normal-order 42))
  (test '(λ x x) (reduce old/normal-order '(λ x x)))
  (test 42 (reduce old/normal-order '($ (λ x x) 42)))
  (test 42 (reduce old/normal-order '($ ($ (λ x x) (λ y y)) 42))))
