\input supp-pdf

\def\title{Moreso}
\def\topofcontents{\null\vfill
  \centerline{\titlefont Moreso}
  \vskip 15pt
  \vfill}
\def\botofcontents{\vfill
%...
}

@* Preamble. Moreso is written as an R6RS Scheme library.

@p
(library (moreso)
  (export constructor? hole? Type?
          Input parse-Input
          Kernel unparse-Kernel
          Input->Kernel)

@ Nanopass.  Moreso relies heavily on the Nanopass Compiler Library
[1, 2, 3, 4] available at
{\tt https://github.com/nanopass/nanopass-framework-scheme}.  Fitting with
nanopass's philosophy, Moreso's passes are small, single-purpose, and
hopefully easily understandable.

@p
  (import (rnrs) (nanopass))

@ Testing.  As we go, we make assertions to spot check that things are
working the way we intend.  Here is the preamble for our test file:

@(tests.ss@>=
(import (rnrs) (moreso) (nanopass))

@* The Kernel Language.  The kernel of Moreso consists of seven forms: four
for typing, two for working with values, function abstraction and application.

@ |Type|.  The symbol ``|Type|'' is a terminal in our core language, so we
need a predicate for our nanopass library.

@p
(define (Type? x)
  (equal? 'Type x))

@ |Type?| works.
@(tests.ss@>=
(assert (Type? 'Type))
(assert (not (Type? 'random)))

@ Contsructors.  Our core language can introduce and eliminate algebraic
types.  The constructors are not named, instead we introduce values of a
particular type with {\tt introduce\_}$n$, where $n\geq 0$.  The first
parameter is the type for which a value is being introduced.

@p
(define (constructor? x)
  (define prefix "introduce_")
  (define prefix-len (string-length prefix))
  (define (all-digits? s)
    (call/cc (lambda (cont)
               (string-for-each
                 (lambda (ch)
                   (if (char<? ch #\0)
                     (cont #f))
                   (if (char<? #\9 ch)
                     (cont #f)))
                 s)
               #t)))
  (and (symbol? x)
       (let* ((s (symbol->string x))
              (len (string-length s)))
         (and (< prefix-len len)
              (string=? (substring s 0 prefix-len) prefix)
              (all-digits? (substring s prefix-len len))))))

@ |constructor?| works.
@(tests.ss@>=
(assert (constructor? 'introduce_0))
(assert (constructor? 'introduce_1))
(assert (constructor? 'introduce_21))
(assert (not (constructor? 'foo)))
(assert (not (constructor? 'verylongname)))
(assert (not (constructor? 'introduce_)))
(assert (not (constructor? 'introduce_foo)))
(assert (not (constructor? 'introduce_-1)))

@ Holes.  Holes have the form ``{\tt ?foo}'' and signify missing terms.  They
are used internally during type-checking, but can be entered manually by the
developer to indicate that a piece of code is not finished.

@p
(define (hole? x)
  (and (symbol? x)
       (let ((spelling (symbol->string x)))
         (and (<= 2 (string-length spelling))
              (char=? #\? (string-ref spelling 0))))))

@ |hole?| Works.
@(tests.ss@>=
(assert (hole? '?foo))
(assert (hole? '?b))
(assert (not (hole? 'foo)))
(assert (not (hole? '?)))
(assert (not (hole? 42)))

@ Names.  To avoid ambiguity, we cannot use reserved words or constructors
as names.

@p
(define (name? x)
  (and (symbol? x)
       (not (constructor? x))
       (not (hole? x))
       (not (memq x '(Type Forall Inductive is eliminate lambda)))))

@ The Kernel Definition.
@p
(define-language Kernel
  (terminals
    (constructor (ctor))
    (name (x))
    (hole (h))
    (Type (Type)))
  (Expr (e)
    x
    h
    ctor
    Type
    (Forall x e0 e1)
    (Inductive e e* ...)
    (is e0 e1)
    (eliminate e)
    (lambda x e)
    (e0 e1)))

@* Multiple Parameters.  The kernel language is easy for us to compile and
analyze since applications all apply a single parameter and |lambda| always
abstracts over a single parameter.  Since this is ergonomically awkward, we
allow entered programs to have multiple-parameter applications and
abstractions, and we boil these down to our kernel language with some compiler
passes.

@ Applications.  Note that we don't allow {\tt ()} (empty parens).
Some lisps allow this as a convenient way to specify a literal empty list,
since it cannot be interpreted as a function call; however, we don't have a
useful empty list value at this point.

@p
(define-language Lmulti-apply
  (extends Kernel)
  (Expr (e)
    (- (e0 e1))
    (+ (e0 e* ...))))

(define-pass simplify-applications : Lmulti-apply (e) -> Kernel ()
  (Expr : Expr (e) -> Expr ()
    [(,[e0] ,[e*] ...) (let loop ((e0 e0)
                                  (e* e*))
                         (if (null? e*)
                           e0
                           (loop `(,e0 ,(car e*)) (cdr e*))))])
  (Expr e))

@ Simplifying Applications Works.
@(tests.ss@>=
(define (parses? from to)
  (equal? to (unparse-Kernel (Input->Kernel (parse-Input from)))))
(define-syntax parses
  (syntax-rules (=>)
    ((parses from => to)
     (assert (parses? (quote from) (quote to))))
    ((parses from => to more ...)
     (begin
       (assert (parses? (quote from) (quote to)))
       (parses more ...)))))

(parses
  (Type Type Type) => ((Type Type) Type)
  (Type Type)      => (Type Type)
  (Type)           => Type)

@ Lambdas.  Multi-parameter lambdas can have any number of parameters,
including zero.  Zero parameter lambdas are removed, since our language is
fully lazy.

@p
(define-language Lmulti-lambda
  (extends Lmulti-apply)
  (Expr (e)
    (- (lambda x e))
    (+ (lambda (x* ...) e))))

(define-pass simplify-lambdas : Lmulti-lambda (e) -> Lmulti-apply ()
  (Expr : Expr (e) -> Expr ()
    [(lambda (,x* ...) ,[e]) (let loop ((e e)
                                        (x* x*))
                               (if (null? x*)
                                 e
                                 `(lambda ,(car x*) ,(loop e (cdr x*)))))])
  (Expr e))

@ Simplifying Lambdas Works.
@(tests.ss@>=
(parses
  (lambda () Type)    => Type
  (lambda (x) Type)   => (lambda x Type)
  (lambda (x y) Type) => (lambda x (lambda y Type)))

@* The Input Language.  The language |Input| changes nothing, but we define it
here so that we don't have to expose the name of the first compiler stage.

@p
(define-language Input
  (extends Lmulti-lambda))

(define-pass input : Input (e) -> Lmulti-lambda ()
  (Expr : Expr (e) -> Expr ())
  (Expr e))

(define-parser parse-Input Input)

@ The Whole Compiler.  Here we string together all the passes for outside
consumption.

@p
(define (Input->Kernel e)
  (simplify-applications (simplify-lambdas (input e))))

@* Rules.

%introduce-sym  := 'introduce_' <digit>
%introduce-expr := (introduce-sym expr_0 ... expr_n)
%eliminate-expr := ('eliminate' expr f_0 ... f_n)
%expr           := introduce-expr
%               |  eliminate-expr

\def\keyword#1{{\tt #1}}
\def\app#1{{\tt(}#1{\tt)}}

\def\Forall{\keyword{Forall}}
\def\Inductive{\keyword{Inductive}}
\def\Type{\keyword{Type}}

\def\isatype{{\rm\ is\ a\ type}}

$$\over\Gamma\vdash\Type\isatype \eqno (TF)$$

$$
  \Gamma\vdash A\isatype
  \quad
  \Gamma,x:A\vdash B\isatype
\over
  \Gamma\vdash{\app{\Forall\ x\ A\ B}}\isatype
\eqno (\forall F)
$$

$$
 \Gamma\vdash A\isatype\quad
 \Gamma,x:A\vdash B\isatype\quad
 \Gamma,x:A\vdash e : B
\over
 \Gamma\vdash \app{\keyword{lambda}\ x\ e} : \app{\Forall\ x\ A\ B}
\eqno (\lambda I)
$$

$$
 \Gamma\vdash A\isatype\quad
 \Gamma\vdash x:A
\over
 \Gamma\vdash \app{\keyword{is}\ A\ x} : A
\eqno (is)
$$

$$
 \Gamma\vdash e_1 : \app{\Forall\ x\ A\ B}\quad
 \Gamma\vdash e_2 : A
\over
 \Gamma\vdash \app{e_1\ e_2} : B[e_2/x]
\eqno (app)
$$

@p
)

@* References.

[1] A. Keep and R. K. Dybvig. A Nanopass Compiler for Commercial Compiler Development. In ICFP '13: Proceedings of the 18th ACM SIGPLAN International Conference on Functional Programming, New York, NY, USA, 2013. ACM.

[2] A. Keep. A Nanopass Framework for Commercial Compiler Development. Doctoral dissertation, Indiana University, Bloomington, Indiana, USA, Feb. 2013.

[3] D. Sarkar. Nanopass Compiler Infrastructure. Doctoral dissertation, Indiana University, Bloomington, Indiana, USA, 2008.

[4] D. Sarkar, O. Waddell, and R. K. Dybvig. A nanopass infrastructure for compiler education. In ICFP '04: Proceedings of the ninth ACM SIGPLAN International Conference on Functional Programming, pages 201–212, New York, NY, USA, 2004. ACM.
