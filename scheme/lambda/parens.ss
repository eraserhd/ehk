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
      [(,[parenthesize -> A] ,rest ...) (parenthesize-applications `($ ,first ,A) rest)]))

  (define (parenthesize expr)
    (match expr
      [(,x) x]
      [(,b . ,[body]) (guard (binding b)) `(λ ,(binding b) ,body)]
      [(,[A] . ,rest) (parenthesize-applications A rest)]
      [,x x]))

  ;; #t if appending a term to E (without further parenthesization) would make
  ;; that term a parameter application to E
  (define (applicable? E)
    (and (list? E) (not (find binding E))))

  (define (ensure-list x)
    (if (list? x)
      x
      (list x)))

  (define (deparenthesize expr)
    (match expr
      [(λ ,[make-binding -> x] ,[F]) `(,x . ,(ensure-list F))]
      [($ ,[a] ,[b]) (if (applicable? a)
		       (append a (ensure-list b))
		       (cons a (ensure-list b)))]
      [,x x]))

  )
