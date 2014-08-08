;;;; program.scm - expand (extended) SRFI-7 specification
;
; match.scm misc.scm syntax.scm eval.scm


; Syntax:
;
; PROGRAM = (program CLAUSE ...)
;
; CLAUSE = (requires REQ ...)                      tests whether features are available
;        | (libraries FILE ...)                    include libraries found in (library-path)
;        | (files FILE ...)                        include files
;        | (code EXPR ...)                         include code
;        | ([feature-]cond (REQ CLAUSE ...) ... [(else CLAUSE ...)])   process clauses depending on available features
;        | (when REQ CLAUSE ...)                   convenience syntax
;        | (unless REQ CLAUSE ...)                 convenience syntax
;
; REQ = ID
;     | (and ID ...)
;     | (or ID ...)
;     | (not ID)
;
; FILE = STRING | SYMBOL | (FILE ...)


(define library-path
  (let ((path (list (or (%get-environment-variable "SCHEME_LIBRARY_PATH")
			(string-append
			 (or (%get-environment-variable "HOME") ".")
			 "/.scheme/lib"))
		    ".")))
    (lambda val
      (if (null? val)
	  path
	  (set! path (car val))))))

(define (program-file-filename x)
  (cond ((string? x) x)
	((symbol? x) (symbol->string x))
	((list? x) (join (map program-file-filename x) "/"))
	((not (pair? x)) (error "invalid filename" x))
	(else (stringify x))))

(define (locate-library fn)
  (or (let ((fn (program-file-filename fn)))
	(any (lambda (ip)
	       (or (%file-exists? (string-append ip "/" fn ".scm"))
		   (%file-exists? (string-append ip "/" fn))))
	     (library-path)))
      (%file-exists? (string-append fn ".scm"))
      (%file-exists? fn)
      (error "library not found" fn)))

(define (expand-program prg . src)
  (let ((src (optional src #f)))
    (define (localize path dir)
      (let ((path (program-file-filename path)))
	(if (or (not dir)
		(and (positive? (string-length path))
		     (memq (string-ref path 0) '(#\\ #\/))))
	    path
	    (string-append dir "/" path))))
    (define (dirname path)
      (let ((len (string-length path)))
	(let loop ((i (- len 1)))
	  (cond ((negative? i) ".")
		((memq (string-ref path i) '(#\\ #\/))
		 (substring path 0 i))
		(else (loop (- i 1)))))))
    (define expand-req
      (match-lambda
	(('and reqs ...)
	 (every expand-req reqs))
	(('or reqs ...)
	 (any expand-req reqs))
	(('not req) 
	 (not (expand-req (list req))))
	((? symbol? r)
	 (memq r %features))
	(r (error "invalid feature requirement" r))))
    (define (expand-clause clause)
      (match clause
	(('requires ids ...)
	 (for-each
	  (lambda (id)
	    (unless (memq id %features)
	      (error "required feature not available" id)))
	  ids)
	 '(void))
	(('libraries names ...)
	 `(begin
	    ,@(map (lambda (name) (read-forms (locate-library name) %read)) names)))
	(('files fns ...)
	 `(begin
	    ,@(map (lambda (fn) (read-forms (localize fn src) read)) fns)))
	(('code exps ...)
	 `(begin ,@exps))
	(((or 'feature-cond 'cond) clauses ...)
	 (let loop ((cs clauses))
	   (match cs
	     (() (error "no requirement satisfied" (map car clauses)))
	     ((('else clauses ...))
	      `(begin ,@(map expand-clause clauses)))
	     (((req clauses ...) . more)
	      (if (expand-req req)
		  `(begin ,@(map expand-clause clauses))
		  (loop more)))
	     (c (error "invalid clause syntax" c)))))
	(('when req clauses ...)
	 (expand-clause `(cond (,req ,@clauses) (else (code (%void))))))
	(('unless req clauses ...)
	 (expand-clause `(when (not ,req) ,@clauses)))
	(_ (error "invalid program clause" clause))))
    (set! src (and src (string? src) (dirname src)))
    (match prg
      (('program clauses ...)
       `(begin ,@(map expand-clause clauses)))
      (_ (error "invalid program form" prg)))))

(define (load-program filename . evaluator)
  (fluid-let ((*current-source-filename* filename))
    ((optional evaluator %eval)
     (expand-program 
      (call-with-input-file filename (lambda (in) (read in)))
      filename))))

(define (%library name)
  (locate-library (stringify name)))
