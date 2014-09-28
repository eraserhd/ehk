(module ehk.lambda.norm (binding normalize denormalize)

  (import chicken scheme)
  (use matchable srfi-1)

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

  ;; #t if appending a term to E (without further parenthesization) would make
  ;; that term a parameter application to E
  (define (applicable? E)
    (cond
      [(not (list? E)) #f]
      [(find binding E) #f]
      [else #t]))

  (define denormalize
    (match-lambda
      [('\\ x (F ...)) (cons (make-binding x) (denormalize F))]
      [('\\ x F) (list (make-binding x) (denormalize F))]
      [('/ a b) (let* ((a* (denormalize a))
		       (b* (denormalize b))
		       (b-list (if (list? b*) b* (list b*))))
		  (if (applicable? a*)
		    (append a* b-list)
		    (cons a* b-list)))]
      [x x]))

  )
