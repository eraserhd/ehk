(include "expect.scm")
(include "../src/moreso.scm")

(define e '(((forty-two . 42))))

(expect (equal? 42 (moreso:eval 42 e)))
(expect (equal? #f (moreso:eval #f e)))
(expect (equal? #t (moreso:eval #t e)))
(expect (equal? 79 (moreso:eval '(if #t 79) e)))
(expect (equal? 86 (moreso:eval '(if #f 79 86) e)))
(expect (equal? 42 (moreso:eval 'forty-two e)))
