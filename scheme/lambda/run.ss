(library (lambda run)
  (export run)
  (import (rnrs)
          (lambda boolean)
          (lambda parens)
          (lambda reduce))

  (define (run E)
    (reduce normal-order (booleans (parenthesize E))))

  )
