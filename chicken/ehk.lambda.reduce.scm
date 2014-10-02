(module ehk.lambda.reduce (E<M/x>
			   free?
			   redex
			   α-convert
			   β-reduce
			   η-reduce
			   reduce
			   old/normal-order)

  (import chicken scheme)
  (use matchable)

  (define (free? E x)
    (match E
      [('λ y F) (if (eq? x y) #f (free? F x))]
      [('$ A B) (or (free? A x) (free? B x))]
      [y (eq? x y)]))

  (define (E<M/x> E M x)
    (match E
      [('$ A B) `($ ,(E<M/x> A M x) ,(E<M/x> B M x))]
      [('λ y F) (cond
		   [(eq? x y) E]
		   [(and (free? M y) (free? F x))
		    (let ((z (gensym)))
		      `(λ ,z ,(E<M/x> (E<M/x> F z y) M x)))]
		   [else `(λ ,y ,(E<M/x> F M x))])]
      [y (if (eq? x y) M y)]))

  (define (α-convert E x)
    (match E
      [('λ y F) `(λ ,x ,(E<M/x> F x y))]))

  (define (β-reduce E)
    (match E
      [`($ (λ ,x ,F) ,G) (E<M/x> F G x)]))

  (define (η-reduce E)
    (match E
      [`(λ ,x ($ ,F ,x)) 
	(if (free? F x)
	  E
	  F)]))

  (define (redex E)
    (match E
      [`($ (λ ,_ ,_) ,_) β-reduce]
      [`(λ ,x ($ ,F ,x)) η-reduce]
      [_ #f]))

  (define (old/normal-order E)
    (match E
      [`($ (λ ,_ ,_) ,_) (β-reduce E)]
      [`($ ,F ,G) `($ ,(old/normal-order F) ,G)]
      [x x]))

  (define (reduce step E)
    (let loop ((E E))
      (match E
        [('$ . _) (loop (step E))]
	[x x])))

  )
