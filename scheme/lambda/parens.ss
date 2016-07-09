(library (lambda parens)
  (export parenthesize deparenthesize)
  (import (rnrs)
	  (utils match))

  (define (binding term)
    (and (symbol? term)
	 (let* ((spelling (symbol->string term))
		(len (string-length spelling)))
	   (and (>= len 3)
	        (char=? #\λ (string-ref spelling 0))
		(char=? #\. (string-ref spelling (- len 1)))
		(string->symbol (substring spelling 1 (- len 1)))))))

  (define (make-binding term)
    (string->symbol (string-append "λ" (symbol->string term) ".")))

  (define (parenthesize-applications first rest)
    (match rest
      [() first]
      [(,b ,_ ...) (guard (binding b)) `($ ,first ,(parenthesize rest))]
      [(,A ,rest ...) (parenthesize-applications `($ ,first ,(parenthesize A)) rest)]))

  (define (parenthesize expr)
    (match expr
      [(,x) x]
      [(,b ,body ...) (guard (binding b)) `(λ ,(binding b) ,(parenthesize body))]
      [(,A ,rest ...) (parenthesize-applications (parenthesize A) rest)]
      [,x x]))

  ;; #t if appending a term to E (without further parenthesization) would make
  ;; that term a parameter application to E
  (define (applicable? E)
    (cond
      [(not (list? E)) #f]
      [(find binding E) #f]
      [else #t]))

  (define (deparenthesize expr)
    (match expr
      [(λ ,x (,F ...)) (cons (make-binding x) (deparenthesize F))]
      [(λ ,x ,F) (list (make-binding x) (deparenthesize F))]
      [($ ,a ,b) (let* ((a* (deparenthesize a))
		        (b* (deparenthesize b))
		        (b-list (if (list? b*) b* (list b*))))
		  (if (applicable? a*)
		    (append a* b-list)
		    (cons a* b-list)))]
      [,x x]))

  )
