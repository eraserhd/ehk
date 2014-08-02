;;;; main.scm - wrapper code for CHICKEN-grass


(declare
  (block)
  (uses library numbers)
  ;(no-bound-checks)
  ;(no-procedure-checks)
  (disable-interrupts))


(import numbers)

(include "chicken/base.scm")
(include "build/core.scm")

(main (command-line-arguments))
