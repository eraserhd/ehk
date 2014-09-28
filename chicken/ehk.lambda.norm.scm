(require-extension matchable)

(module ehk.lambda.norm (binding normalize)

  (import scheme matchable)

  (define (binding term)
    (and (symbol? term)
	 (let* ((spelling (symbol->string term))
		(len (string-length spelling)))
	   (and (>= len 3)
	        (char=? #\\ (string-ref spelling 0))
		(char=? #\. (string-ref spelling (- len 1)))
		(string->symbol (substring spelling 1 (- len 1)))))))

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
  
  )
