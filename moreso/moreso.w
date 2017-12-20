\input supp-pdf

\def\title{Moreso}
\def\topofcontents{\null\vfill
  \centerline{\titlefont Moreso}
  \vskip 15pt
  \vfill}
\def\botofcontents{\vfill
%...
}

@* Preamble.  Moreso is written as an R6RS Scheme library.

@p
(library (moreso)
  (export Type?)
  (import (rnrs) (nanopass))

@ Testing.  As we go, we make assertions to spot check that things are
working the way we intend.  Here is the preamble for our test file:

@(tests.ss@>=
(import (rnrs) (moreso))

@* The Kernel Language.  The kernel of Moreso consists of seven forms: four
for typing, two for working with values, function abstraction and application.

\medskip
$\bullet$ {\tt Inductive}\par
\medskip
$\bullet$ ${\tt introduce}_i$\par
$\bullet$ {\tt eliminate}\par
\medskip

@ |Type|.  The symbol ``|Type|'' is a terminal in our core language, so we
need a predicate for our nanopass library.

@p
(define (Type? x)
  (equal? 'Type x))

@ |Type?| works.
@(tests.ss@>=
(assert (Type? 'Type))
(assert (not (Type? 'random)))

@ The Kernel Definition.
@p
(define-language Kernel
  (terminals
    (symbol (x))
    (Type (Type)))
  (Expr (e)
    Type
    (Forall x e0 e1)
    (is e0 e1)
    (lambda x e)
    (e0 e1)))

@* Rules. Are here.

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
