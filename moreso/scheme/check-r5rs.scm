
; Match buffers are ring buffers which allows us to easily test if the last N
; characters matched some string.  The ring buffer must be larger than any
; string we will attempt to match.

(define (make-match-buffer size)
  (cons 0 (make-vector size #f)))

(define (match-buffer-length buffer)
  (vector-length (cdr buffer)))

(define (match-buffer-index buffer)
  (car buffer))

(define (match-buffer-ref buffer n)
  (let ((index (modulo (+ (car buffer) n) (match-buffer-length buffer))))
    (vector-ref (cdr buffer) index)))

(define (match-buffer-put! buffer c)
  (vector-set! (cdr buffer) (car buffer) c)
  (set-car! buffer (modulo (+ 1 (car buffer)) (match-buffer-length buffer))))

(define (match-buffer-matches? buffer str)
  (let loop ((str-i 0)
	     (buffer-i (- (match-buffer-length buffer) (string-length str))))
    (cond
      ((= str-i (string-length str))
       #t)
      ((eqv? (string-ref str str-i)
	     (match-buffer-ref buffer buffer-i))
       (loop (+ 1 str-i) (+ 1 buffer-i)))
      (else
	#f))))

; A PORT-STACK is a LIFO stack of ports.  When we reach the end of one port,
; we pop it from the stack and continue reading where we left off.  This
; allows us to process \input{} directives transparently.

(define (make-port-stack port)

  (define stack (list port))
  
  (define (push! port)
    (set! stack (cons port stack)))
  (define (pop!)
    (set! stack (cdr stack)))

  (define eof-object #f)

  (define (read-char-and-save-eof port)
    (let ((answer (read-char port)))
      (if (eof-object? answer)
	(set! eof-object answer))
      answer))

  (define (next-char)
    (if (null? stack)
      eof-object
      (let ((char (read-char-and-save-eof (car stack))))
	(if (eof-object? char)
	  (begin
	    (pop!)
	    #\newline)
	  char))))

  (vector next-char push!))

(define (port-stack-read-char port-stack)
  ((vector-ref port-stack 0)))

(define (port-stack-push! port-stack port)
  ((vector-ref port-stack 1) port))

; An INPUT-PROCESSOR accept characters and, when it detects it has received
; the end of an \input{} directive, it calls INPUT-PROC with the name of
; the file specified in the directive.

(define (make-input-processor input-proc)

  (define match-buffer (make-match-buffer 7))
  (define (put! char)
    (match-buffer-put! match-buffer char))
  (define (matches? str)
    (match-buffer-matches? match-buffer str))

  (define reading-filename? #f)
  (define filename '())

  (define (accept-char char)
    (put! char)
    (cond
      ((and reading-filename? (char=? char #\}))
       (set! reading-filename? #f)
       (input-proc (list->string (reverse filename))))

      (reading-filename?
       (set! filename (cons char filename)))

      ((matches? "\\input{")
       (set! reading-filename? #t)
       (set! filename '()))))

  accept-char)

; A TEX-READER reads the TeX source, following \input{} directives.

(define (make-tex-reader port path-prefix)

  (define stack (make-port-stack port))
  (define input-processor (make-input-processor
			    (lambda (filename)
			      (let* ((full-path (string-append path-prefix filename ".tex"))
				     (port (open-input-file full-path)))
				(port-stack-push! stack port)))))

  (define (next-char)
    (let ((char (port-stack-read-char stack)))
      (input-processor char)
      char))

  next-char)

