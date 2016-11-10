(library (logic repl)
  (export prove intro)
  (import (rnrs)
          (utils match))

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
    (display-status))

  (define (intro)
    (assert *goals*)
    (set! *goals* (match *goals*
                    [(((,A => ,B) ,assuming ...) ,rest ...)
                     `((,B ,A ,@assuming) ,@rest)]))
    (display-status)))
