(include "expect.scm")
(include "../src/moreso.scm")

(expect (equal? 42 (moreso:eval 42)))
(expect (equal? 79 (moreso:eval '(if #t 79))))
