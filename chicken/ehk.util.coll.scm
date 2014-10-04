(module ehk.util.coll (update update-in)
  (import scheme chicken)

  (define (update lst n fn)
    (if (= 0 n)
      (cons (fn (car lst)) (cdr lst))
      (cons (car lst) (update (cdr lst) (- n 1) fn))))

  (define (update-in E path reducer)
    (if (null? path)
      (reducer E)
      (update E (car path) (cut update-in <> (cdr path) reducer))))

  )
