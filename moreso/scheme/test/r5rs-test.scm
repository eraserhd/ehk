(include "expect.scm")
(include "../src/moreso.scm")

(expect (= 42 (moreso:eval '(let ((x 42)) x) moreso:r5rs)))
