
(define *messages*
  '(((offer is secret) . #t)
    ((click secret link) . #t)
    ((secret sports link) . #t)
    ((play sports today) . #f)
    ((went play sports) . #f)
    ((secret sports event) . #f)
    ((sports is today) . #f)
    ((sports costs money) . #f)))

;; Given a message set, build a spam classifier.
(define (compile-messages messages)
  (define P-spam (/ (length (filter cdr messages)) (length messages)))
  (define P-ham (- 1.0 P-spam))

  (define unique-words
    (let loop ((input (apply append (map car messages)))
	       (output '()))
      (cond
	((null? input)
	 output)
	((memq (car input) output)
	 (loop (cdr input) output))
	(else
	 (loop (cdr input) (cons (car input) output))))))

  (define (all-words spam?)
    (apply append
      (map car
	(filter (lambda (x) (eq? (cdr x) spam?)) messages))))

  (define (word-count word spam?)
    (let loop ((count 0)
	       (all-words (all-words spam?)))
      (let ((next (memq word all-words)))
	(if next
	  (loop (+ 1 count) (cdr next))
	  count))))
    
  (define (build-P-word-given messages spam?)
    (define all-words-count (length (all-words spam?)))
    (let loop ((words unique-words)
	       (frequency-table '()))
      (if (null? words)
	frequency-table
	(loop
	  (cdr words)
	  (cons
	    (cons (car words) (/ (word-count (car words) spam?) all-words-count))
	    frequency-table)))))

  (define P-word-given-spam (build-P-word-given messages #t))
  (define P-word-given-ham (build-P-word-given messages #f))

  (define (classify message)
    (let classify-loop ((P-spam P-spam)
			(message message))
      (if (null? message)
	P-spam
	(let* ((P-ham (- 1.0 P-spam))
	       (P-W-given-spam (cdr (assq (car message) P-word-given-spam)))
	       (P-W-given-ham (cdr (assq (car message) P-word-given-ham))))
	  (classify-loop
	    (/ (* P-W-given-spam P-spam)
	       (+ (* P-W-given-spam P-spam)
		  (* P-W-given-ham P-ham)))
	    (cdr message))))))

  classify)


  
