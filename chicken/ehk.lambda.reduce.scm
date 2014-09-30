(module ehk.lambda.reduce (E<M/x> free? η-reduce)

  (import chicken scheme)
  (use matchable)

  (define (free? E x)
    (match E
      [('\\ y F) (if (eq? x y) #f (free? F x))]
      [('/ A B) (or (free? A x) (free? B x))]
      [y (eq? x y)]))

  (define (E<M/x> E M x)
    (match E
      [('/ A B) `(/ ,(E<M/x> A M x) ,(E<M/x> B M x))]
      [('\\ y F) (cond
		   [(eq? x y) E]
		   [(and (free? M y) (free? F x))
		    (let ((z (gensym)))
		      `(\\ ,z ,(E<M/x> (E<M/x> F z y) M x)))]
		   [else `(\\ ,y ,(E<M/x> F M x))])]
      [y (if (eq? x y) M y)]))

  (define (η-reduce E)
    (match E
      [`(\\ ,(? symbol? x) (/ ,F ,x)) 
	(if (free? F x)
	  E
	  (η-reduce F))]
      [F F]))

  )
