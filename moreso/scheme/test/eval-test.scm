(include "expect.scm")
(include "../src/moreso.scm")

(expect (= 42 (moreso:eval 42)))
