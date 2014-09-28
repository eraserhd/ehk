(require-extension matchable)

(module ehk.lambda.norm (binding?)

  (import scheme matchable)

  (define (binding? term)
    (and (symbol? term)
	 (let ((s (symbol->string term)))
	   (and (char=? #\\ (string-ref s 0))
		(char=? #\. (string-ref s (- (string-length s) 1)))))))
  
  )
