\input supp-pdf

\def\title{Moreso}
\def\topofcontents{\null\vfill
  \centerline{\titlefont Moreso}
  \vskip 15pt
  \vfill}
\def\botofcontents{\vfill
%...
}

@* The Kernel Language.  The kernel of Moreso consists of seven forms: four
for typing, two for working with values, function abstraction and application.

\medskip
$\bullet$ {\tt Type}\par
$\bullet$ {\tt Forall}\par
$\bullet$ {\tt Inductive}\par
$\bullet$ {\tt is}\par
\medskip
$\bullet$ ${\tt introduce}_i$\par
$\bullet$ {\tt eliminate}\par
\medskip
$\bullet$ {\tt lambda}\par
$\bullet$ function application\par
\medskip

In the kernel language, lambdas have a single parameter and applications
apply a single accepts them and
converts them.argument.  This is convenient for writing a complier, but
inconvenient for writing programs, so non-kernel Moreso accepts them and
converts them.

@(moreso.ss@>=
(library (moreso)
  (export desugar)
  (import (rnrs) (nanopass))

  (define (desugar form)
    #f)
)
@* Tests.  Our tests are simple assertions---spot checks, really---that
our code does what we claim it does so far.

@(tests.ss@>=
(import (rnrs) (moreso))
@ Sample Check.  We do this.
@(tests.ss@>=
(assert #t)
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

@* Inductive Types.  Moreso uses a general mechanism for inductive types
based on Luo's implementation of UTT.

@ Strict Positivity of Operators.  Operators are functional types which
produce a value of some type.

@c () => (schema-positive?)
@<Definition of schema-positive?@>=
(define (schema-positive? type-name schema)
  ...)
