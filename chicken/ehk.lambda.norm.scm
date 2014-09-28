(require-extension matchable)

(module ehk.lambda.norm (binding normalize denormalize)

  (import scheme matchable)

  (define (binding term)
    (and (symbol? term)
	 (let* ((spelling (symbol->string term))
		(len (string-length spelling)))
	   (and (>= len 3)
	        (char=? #\\ (string-ref spelling 0))
		(char=? #\. (string-ref spelling (- len 1)))
		(string->symbol (substring spelling 1 (- len 1)))))))

  (define (make-binding term)
    (string->symbol (string-append "\\" (symbol->string term) ".")))

  (define (normalize-applications first rest)
    (match rest
      [() first]
      [((? binding) _ ...) `(/ ,first ,(normalize rest))]
      [(A rest ...) (normalize-applications `(/ ,first ,(normalize A)) rest)]))

  (define normalize
    (match-lambda
      [(x) x]
      [((? binding b) body ...) `(\\ ,(binding b) ,(normalize body))]
      [(A rest ...) (normalize-applications (normalize A) rest)]
      [x x]))

  (define (contains-binding? l)
    (match l
      [((? binding) _ ...) #t]
      [(_ rest ...) (contains-binding? rest)]
      [_ #f]))

  (define denormalize
    (match-lambda
      [('\\ x (F ...)) (cons (make-binding x) (denormalize F))]
      [('\\ x F) (list (make-binding x) (denormalize F))]
      [('/ a b) (let ((a (denormalize a))
		      (b (denormalize b)))
		  (cond
		    [(contains-binding? a) (list a b)]
		    [(list? a) (append a (list b))]
		    [(list? b) (cons a b)]
		    [else (cons a (list b))]))]
      [x x]))

  
  )
