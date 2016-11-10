(library (logic print)
  (export format-proposition)
  (import (rnrs)
          (utils match))

  (define (format-proposition P)
    (call-with-string-output-port 
      (lambda (port)
        (write P port)))))
