(require-extension matchable)

(module ehk.lambda.norm (binding normalize)

  (import scheme srfi-1 matchable)

  (define (binding term)
    (and (symbol? term)
	 (let* ((spelling (symbol->string term))
		(len (string-length spelling)))
	   (and (>= len 3)
	        (char=? #\\ (string-ref spelling 0))
		(char=? #\. (string-ref spelling (- len 1)))
		(string->symbol (substring spelling 1 (- len 1)))))))

  (define (normalize E)
    (cond
      [(not (list? E)) E]
      [(= 1 (length E)) (car E)]
      [(binding (car E)) `(\\ ,(binding (car E)) ,(normalize (cdr E)))]
      [else
	(let loop ((terms (cdr E))
		   (applications (normalize (car E))))
	  (cond
	    [(null? terms) applications]
	    [(binding (car terms)) `(/ ,applications ,(normalize terms))]
	    [else (loop (cdr terms) `(/ ,applications ,(normalize (car terms))))]))]))
  
  )
