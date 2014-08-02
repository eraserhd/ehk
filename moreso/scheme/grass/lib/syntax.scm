;;;; syntax.scm - various useful macros, used in the core system


(define-syntax define-syntax-rule
  (syntax-rules ___ ()
    ((_ (name args ___) rule)
     (define-syntax name
       (syntax-rules ()
	 ((_ args ___) rule))))))

(define-syntax-rule (when x y z ...)
  (if x (begin y z ...)))

(define-syntax-rule (unless x y z ...)
  (if (not x) (begin y z ...)))

(define-syntax cut
  (syntax-rules (<> <...>)
    ;; construct fixed- or variable-arity procedure:
    ((_ "1" (slot-name ...) (proc arg ...))
     (lambda (slot-name ...) (proc arg ...)))
    ((_ "1" (slot-name ...) (proc arg ...) <...>)
     (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
    ;; process one slot-or-expr
    ((_ "1" (slot-name ...)   (position ...)      <>  . se)
     (cut "1" (slot-name ... x) (position ... x)        . se))
    ((_ "1" (slot-name ...)   (position ...)      nse . se)
     (cut "1" (slot-name ...)   (position ... nse)      . se))
    ((_ . slots-or-exprs)
     (cut "1" () () . slots-or-exprs))) )

(define-syntax fluid-let
  (syntax-rules ()
    ((_ ((v1 e1) ...) b1 b2 ...)
     (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
    ((_ "temps" (t ...) ((v1 e1) x ...) b1 b2 ...)
     (let ((temp e1))
       (fluid-let "temps" ((temp e1 v1) t ...) (x ...) b1 b2 ...)))
    ((_ "temps" ((t e v) ...) () b1 b2 ...)
     (let-syntax ((swap!
                   (syntax-rules ()
                     ((swap! a b)
                      (let ((tmp a))
                        (set! a b)
                        (set! b tmp))))))
       (dynamic-wind
	   (lambda () (swap! t v) ...)
	   (lambda () b1 b2 ...)
	   (lambda () (swap! t v) ...))))))

(define-syntax-rule (begin0 x1 x2 ...)
  (call-with-values (lambda () x1)
    (lambda results
      x2 ...
      (apply values results))))

(define-syntax optional
  (syntax-rules ()
    ((_ x y) (if (pair? x) (car x) y))
    ((_ x) (optional x #f))))

(define-syntax let-optionals
  (syntax-rules ()
    ((_ rest () body ...) (let () body ...))
    ((_ rest ((var default) . more) body ...)
     (let* ((tmp rest)
	    (var (if (null? tmp) default (car tmp)))
	    (rest2 (if (null? tmp) '() (cdr tmp))) )
       (let-optionals rest2 more body ...) ) )
    ((_ rest (var) body ...) (let ((var rest)) body ...)) ) )

(define-syntax assert
  (syntax-rules ()
    ((_ x) (assert x "assertion failed" 'x))
    ((_ x args ...) 
     (let ((tmp x))
       (unless tmp (error args ...))
       tmp))))

(define-syntax define-values
  (syntax-rules ()
    ((_ "1" () exp ((var tmp) ...))
     (define
       (call-with-values (lambda () exp)
	 (lambda (tmp ...)
	   (set! var tmp) ...))))
    ((_ "1" (var . more) exp (binding ...))
     (define-values "1" more exp (binding ... (var tmp))))
    ((_ () exp) 
     (define 
       (call-with-values (lambda () exp)
	 (lambda _ (void)))))
    ((_ (var) exp) 
     (define var exp))
    ((_ (var ...) exp)
     (begin
       (define var #f) ...
       (define-values "1" (var ...) exp ())))))

(define-syntax receive
  (syntax-rules ()
    ((receive expression)
     (call-with-values (lambda () expression) list))
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
       (lambda formals body ...)))))
