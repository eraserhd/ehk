(library (lambda reduce)
  (export E<M/x>
          free?
          redex
          α-convert
          β-reduce
          η-reduce
          reduce
          normal-order)
  (import (rnrs)
          (utils match)
          (only (scheme) gensym))

  (define (free? E x)
    (match E
      [(λ ,y ,F) (guard (eq? x y)) #f]
      [(λ ,y ,[F]) F]
      [($ ,[A] ,[B]) (or A B)]
      [,y (eq? x y)]))

  (define (α-convert E x)
    (match E
      [(λ ,y ,F) `(λ ,x ,(E<M/x> F x y))]))

  (define (E<M/x> E M x)
    (match E
      [($ ,[A] ,[B]) `($ ,A ,B)]
      [(λ ,y ,F) (guard (eq? x y)) `(λ ,y ,F)]
      [(λ ,y ,F) (guard (and (free? M y) (free? F x)))
       (E<M/x> (α-convert `(λ ,y ,F) (gensym)) M x)]
      [(λ ,y ,[F]) `(λ ,y ,F)]
      [,y (if (eq? x y) M y)]))

  (define (β-reduce E)
    (match E
      [($ (λ ,x ,F) ,G) (E<M/x> F G x)]))

  (define (η-reduce E)
    (match E
      [(λ ,x ($ ,F ,x)) 
       (if (free? F x)
         E
         F)]))

  (define (redex E)
    (match E
      [($ (λ ,a ,b) ,c) β-reduce]
      [(λ ,x ($ ,F ,x)) η-reduce]
      [,_ #f]))

  (define (path-prefix n)
    (lambda (result)
      (cons (car result) (cons n (cdr result)))))

  (define (normal-order E)
    (cond
      ((redex E) => list)
      (else
       (match E
        [(,_ ,a ,b) (cond
                      ((normal-order a) => (path-prefix 1))
                      ((normal-order b) => (path-prefix 2))
                      (else #f))]
        [,_ #f]))))

  (define (update lst n fn)
    (if (= 0 n)
      (cons (fn (car lst)) (cdr lst))
      (cons (car lst) (update (cdr lst) (- n 1) fn))))

  (define (update-in E path reducer)
    (if (null? path)
      (reducer E)
      (update E (car path) (lambda (E)
                             (update-in E (cdr path) reducer)))))

  (define (reduce order E)
    (let loop ((E E))
      (let ((found (order E)))
        (if found
          (loop (update-in E (cdr found) (car found)))
          E))))
  
  )
