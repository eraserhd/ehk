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
    (cond
      [(null? rest) first]
      [(binding (car rest)) `(/ ,first ,(normalize rest))]
      [else (normalize-applications `(/ ,first ,(normalize (car rest))) (cdr rest))]))

  (define (normalize E)
    (cond
      [(not (list? E)) E]
      [(= 1 (length E)) (car E)]
      [(binding (car E)) `(\\ ,(binding (car E)) ,(normalize (cdr E)))]
      [else (normalize-applications (normalize (car E)) (cdr E))]))
  
  )
