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

  (define (normalize E)
    `(\\ ,(binding (car E)) ,@(cdr E)))
  
  )
