(include "expect.scm")
(include "../src/moreso.scm")

(expect (equal? 42 (moreso:eval 42)))
(expect (equal? #f (moreso:eval #f)))
(expect (equal? #t (moreso:eval #t)))
(expect (equal? 79 (moreso:eval '(if #t 79))))
(expect (equal? 86 (moreso:eval '(if #f 79 86))))
