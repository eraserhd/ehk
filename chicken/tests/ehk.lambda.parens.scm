
(test-group "fully parenthesizing lambda abstractions"
  (test '(λ x x) (parenthesize '(λx. x)))
  (test '(λ x 1) (parenthesize '(λx. 1)))
  (test '(λ y 4) (parenthesize '(λy. 4)))
  (test '(λ y (λ x 4)) (parenthesize '(λy. λx. 4)))
  (test '($ x y) (parenthesize '(x y)))
  (test '($ ($ x y) z) (parenthesize '(x y z)))
  (test '($ ($ ($ x y) z) w) (parenthesize '(x y z w)))
  (test '($ (λ x x) 4) (parenthesize '((λx. x) 4)))
  (test '($ f (λ x (λ y ($ ($ - y) x)))) (parenthesize '(f λx. λy. - y x))))

(test-group "removing unnecessary parentheses from lambda abstractions"
  (test '(λx. x) (deparenthesize '(λ x x)))
  (test '(λx. 1) (deparenthesize '(λ x 1)))
  (test '(λy. λx. 4) (deparenthesize '(λ y (λ x 4))))
  (test '(x y) (deparenthesize '($ x y)))
  (test '(x y z) (deparenthesize '($ ($ x y) z)))
  (test '(x y z w) (deparenthesize '($ ($ ($ x y) z) w)))
  (test '(f λx. λy. - y x) (deparenthesize '($ f (λ x (λ y ($ ($ - y) x))))))
  (test '((λx. x) 4) (deparenthesize '($ (λ x x) 4)))
  (test '((λx. x) λy. y) (deparenthesize '($ (λ x x) (λ y y)))))
