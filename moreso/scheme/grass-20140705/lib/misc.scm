;;;; misc.scm - miscellaneous utility functions needed for base system, but see stuff for more stuff...
;
; syntax.scm
;
; - this code needs to run both in the host-implementation (with necessary extensions
;   by */base.scm) and in grass itself, so be careful with "%..." primitives.


(define (id x) x)

(define (read-forms file-or-port . reader)
  ((if (string? file-or-port)
       call-with-input-file
       (lambda (fp p) (p fp)))
   file-or-port
   (let ((rd (optional reader read)))
     (lambda (port)
       (let loop ((xs '()))
	 (let ((x (rd port)))
	   (if (eof-object? x)
	       `(begin ,@(reverse xs))
	       (loop (cons x xs)))))))))

(define (emit . xs)
  (for-each display xs))

(define (stringify x)
  (cond ((symbol? x) (symbol->string x))
	((string? x) x)
	((number? x) (number->string x))
	((char? x) (string x))
	(else (error "can't stringify" x))))

(define (symbolify x)
  (cond ((symbol? x) x)
	((string? x) (string->symbol x))
	((char? x) (string->symbol (string x)))
	(else (error "can't symbolify" x))))

(define (listify x)
  (if (list? x) 
      x
      (list x)))

(define (join xs . sep)
  (let ((sep (optional sep "")))
    (apply
     string-append
     (let loop ((xs xs))
       (cond ((null? xs) '())
	     ((null? (cdr xs)) xs)
	     (else (cons (car xs) (cons sep (loop (cdr xs))))))))))

(define (every pred lst)
  (let loop ((lst lst))
    (cond ((null? lst))
	  ((not (pred (car lst))) #f)
	  (else (loop (cdr lst))))))

(define (any pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
	  ((pred (car lst)))
	  (else (loop (cdr lst))))))
