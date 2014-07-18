(define expect:*example-count* 0)
(define expect:*failures* '())

(define-type expect-failure
  (condition read-only:)
  (message read-only:))

(define-macro (expect . args)

  (define condition #f)
  (define message #f)

  (define includes-failure-message?
    (= 2 (length args)))

  (cond
    (includes-failure-message?
     (set! message (car args))
     (set! condition (cadr args)))
    (else
     (set! condition (car args))))
  
  `(begin
     (set! expect:*example-count* (+ 1 expect:*example-count*))
     (if (not ,condition)
       (let ((failure (make-expect-failure ',condition ',message)))
         (set! expect:*failures* (append expect:*failures* (list failure)))
         (display "F"))
       (display "."))))

(define (expect:display-failure failure)
  (display "FAILED: ")
  (if (expect-failure-message failure)
    (begin
      (display (expect-failure-message failure))
      (display " ")))
  (write (expect-failure-condition failure))
  (newline))

(define (expect:display-results)
  (cond
    ((= 0 expect:*example-count*)
     #f)
    (else
     (newline)
     (newline)
     (display expect:*example-count*)
     (display " examples, ")
     (display (length expect:*failures*))
     (display " failures")
     (newline)
     (if (> (length expect:*failures*) 0)
       (begin
         (newline)
         (for-each expect:display-failure expect:*failures*))))))

(define (expect:override-exit-to-display-results)
  (let ((default-exit ##exit))
    (set! ##exit (lambda (#!optional (exit-code 0))
                       (expect:display-results)
                       (default-exit exit-code)))))
  
(expect:override-exit-to-display-results)
