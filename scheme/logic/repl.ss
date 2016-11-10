(library (logic repl)
  (export prove)
  (import (rnrs))

  (define *goals* #f)

  (define (display-active-goal goal)
    (newline)
    (for-each
      (lambda (assumption)
        (display " ")
        (write assumption)
        (newline))
      (cdr goal))
    (display " ---------------------------------------------")
    (newline)
    (display " ")
    (write (car goal))
    (newline))

  (define (display-status)
    (display-active-goal (car *goals*)))

  (define (prove goal)
    (assert (not *goals*))
    (set! *goals* `((,goal)))
    (display-status)))
