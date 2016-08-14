(library (lambda debruijn)
  (export debruijnize)
  (import (rnrs) (utils match))

  (define (level var bindings)
    (let loop ((bindings bindings)
               (n 1))
      (if (eq? (car bindings) var)
        n
        (loop (cdr bindings) (+ 1 n)))))

  (define (debruijnize expr bindings)
    (match expr
      [(λ ,var ,body) `(λ ,(debruijnize body (cons var bindings)))]
      [($ ,[a] ,[b]) `($ ,a ,b)]
      [,v (guard (symbol? v)) (level v bindings)]))

)
