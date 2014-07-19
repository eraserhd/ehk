(include "expect.scm")
(include "../src/moreso.scm")

(define e `(((forty-two . 42)
	     (+ . ,+)
	     (/ . ,/))))

(define-macro (raises? expr)
  `(with-exception-catcher
     (lambda (ex)
       #t)
     (lambda ()
       ,expr
       #f)))

(expect (equal? 42 (moreso:eval 42 e)))
(expect (equal? #f (moreso:eval #f e)))
(expect (equal? #t (moreso:eval #t e)))
(expect (equal? 79 (moreso:eval '(if #t 79) e)))
(expect (equal? 86 (moreso:eval '(if #f 79 86) e)))
(expect (equal? 42 (moreso:eval 'forty-two e)))
(expect (raises? (moreso:eval 'eleventy-seven e)))

;; quote
(expect (equal? '(1 2) (moreso:eval ''(1 2) e)))
(expect (raises? (moreso:eval '(quote (1 2) 1) e)))

;; set!
(expect (let ((e '(((foo . #f)))))
	  (moreso:eval '(set! foo 42) e)
	  (equal? 42 (cdaar e))))
(expect (raises? (moreso:eval '(set! does-not-exist 42) e)))
(expect (raises? (moreso:eval '(set!) e)))
(expect (raises? (moreso:eval '(set! forty-two) e)))

;; native procedure calls
(expect (equal? 42 (moreso:eval '(+ 40 2) e)))
(expect (equal? 20 (moreso:eval '(/ 40 2) e)))
