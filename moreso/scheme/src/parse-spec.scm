(require-extension irregex)

; A SOURCE produces a character with each invocation (or #f at end-of-stream).

(define (port->source port)
  (lambda ()
    (let ((char (read-char port)))
      (if (eof-object? char)
        #f
        char))))

(define (string->source str)
  (define i 0)
  (define (next)
    (if (= i (string-length str))
      #f
      (let ((char (string-ref str i)))
        (set! i (+ 1 i))
        char)))
  next)

(define (slurp source)
  (let loop ((chars '()))
    (let ((char (source)))
      (if char
        (loop (cons char chars))
        (list->string (reverse chars))))))

; FILTER-TEX-COMMENTS is a source which removes %-style comments from the
; TeX stream.

(define (filter-tex-comments source)
  (define state 'initial)
  (define (next)
    (let ((char (source)))
      (cond
        ((not char)
         #f)

        ((and (eq? state 'comment)
              (char=? char #\newline))
         (set! state 'initial)
         (next))

        ((eq? state 'comment)
         (next))

        ((and (eq? state 'initial)
              (char=? char #\%))
         (set! state 'comment)
         (next))

        (else
         char))))

  next)

; A SOURCE-STACK is a stack of sources.  When one reaches end-of-stream, we
; resume reading from the next source.  MAKE-SOURCE-STACK returns a pair
; where the CAR is the compound stream and the and the CDR is a procedure
; for pushing another stream on the stack.

(define (make-source-stack)

  (define stack '())

  (define (pop!)
    (set! stack (cdr stack)))
  (define (push! source)
    (set! stack (cons source stack)))

  (define (next)
    (if (null? stack)
      #f
      (let ((char ((car stack))))
        (if char
          char
          (begin
            (pop!)
            (next))))))

  (cons next push!))

; A MATCH-BUFFER is a ring buffer that can detect if it is filled with a
; particular pattern.

(define (match-buffer pattern)

  (define buffer (make-vector (string-length pattern)))
  (define buffer-i 0)

  (define (put! char)
    (vector-set! buffer buffer-i char)
    (set! buffer-i (modulo (+ buffer-i 1) (string-length pattern))))

  (define (matches?)
    (let loop ((i 0))
      (cond
        ((= i (string-length pattern))
         #t)

        ((eqv? (string-ref pattern i)
               (vector-ref buffer (modulo (+ i buffer-i) (string-length pattern))))
         (loop (+ 1 i)))

        (else
         #f))))

  (cons put! matches?))

; A TEX-INPUT-SOURCE detects "\input{foo}" and pushes files onto the read
; stack.

(define (tex-input-source path initial-file)
  (let* ((input-matcher (match-buffer "\\input{"))
         (put! (car input-matcher))
         (matches? (cdr input-matcher))

         (stack (make-source-stack))
         (stack-source (car stack))
         (push! (cdr stack))
         (expand-filename (lambda (filename)
                            (string-append path filename ".tex")))
         (input! (lambda (filename)
                   (push! (port->source (open-input-file (expand-filename filename))))))

         (state 'initial)
         (input-name-chars '()))
    (input! initial-file)
    (lambda ()
      (let ((char (stack-source)))
        (put! char)
        (cond
          ((not char))

          ((and (eq? state 'initial) (matches?))
           (set! state 'input-name)
           (set! input-name-chars '()))

          ((and (eq? state 'input-name) (char=? char #\}))
           (set! state 'initial)
           (input! (list->string (reverse input-name-chars))))

          ((eq? state 'input-name)
           (set! input-name-chars (cons char input-name-chars))))
        char))))

; An EXAMPLE-EXTRACTOR looks for tagged scheme source examples in the stream
; and collects them.

(define (example-extractor source)
  (let* ((start-matcher (match-buffer "\\begin{scheme}%!example"))
         (start? (cdr start-matcher))

         (end-matcher (match-buffer "\\end{scheme}"))
         (end? (cdr end-matcher))

         (put! (lambda (char)
                 ((car start-matcher) char)
                 ((car end-matcher) char)))

         (state 'initial)
         (example-chars '())
         (examples '()))

    (let char-loop ()
      (let ((char (source)))
        (put! char)
        (cond
          ((not char)
           examples)

          ((and (eq? state 'initial) (start?))
           (set! state 'in-example)
           (set! example-chars '())
           (char-loop))

          ((and (eq? state 'in-example) (end?))
           (set! state 'initial)
           (set! examples (cons (cons #f ;; section number
                                      (list->string (reverse (cdr (memv #\\ example-chars)))))
                                examples))
           (char-loop))

          ((eq? state 'in-example)
           (set! example-chars (cons char example-chars))
           (char-loop))

          (else
           (char-loop)))))))

(define (compile-example example)
  example)

(define (examples)
  (map compile-example (example-extractor (tex-input-source "r5rs/" "r5rs"))))

; vim:set et:
