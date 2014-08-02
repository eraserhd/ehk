;;;; main.scm - startup code code gambit


(declare
  (standard-bindings)
  (extended-bindings)
  (block))

(include "../base.scm")
(include "base.scm")
(include "../build/core.scm")

(main (cdr (command-line)))
