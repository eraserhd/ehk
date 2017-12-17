\input supp-pdf

\def\title{Moreso}
\def\topofcontents{\null\vfill
  \centerline{\titlefont Moreso}
  \vskip 15pt
  \vfill}
\def\botofcontents{\vfill
%...
}

@* The Language.

\verbatim
symbol         := <any non-whitespace, non-parenthesis character>+
is-expr        := ('is' Ty expr)                         ; a type assertion
lambda-expr    := ('lambda' symbol expr)
introduce-sym  := 'introduce_' <digit>+                  ; constructor n for a particular type
introduce-expr := (introduce-sym expr_0 ... expr_n)      ; construct a value
eliminate-expr := ('eliminate' expr f_0 ... f_n)         ; destruct a value
type-expr      := ('inductive' Ty schema_0 ... schema_n) ; introduce a new inductive type

expr           := is-expr
               |  lambda-expr
               |  introduce-expr
               |  eliminate-expr
               |  type-expr
               |  symbol                                 ; variable reference
               |  (expr_1 ... expr_n)                    ; application
!endverbatim

\def\keyword#1{{\tt #1}}
\def\app#1{{\tt(}#1{\tt)}}

\def\Forall{\keyword{Forall}}
\def\Inductive{\keyword{Inductive}}
\def\Type{\keyword{Type}}

\def\isatype{\rm\ is\ a\ type}

$$\over\Gamma\vdash\Type\isatype$$

$$
  \Gamma\vdash A\isatype
  \quad
  \Gamma,x:A\vdash B\isatype
\over
\Gamma\vdash{\app{\Forall\ x\ A\ B}}\isatype
$$

$$ ?? \over \Gamma\vdash \app{\Inductive\ }\isatype $$

$$
 \Gamma\vdash A\isatype\quad
 \Gamma,x:A\vdash B\isatype\quad
 \Gamma,x:A\vdash e : B
\over
 \Gamma\vdash \app{lambda\ x\ e} : \app{\Forall\ x\ A\ B}
$$

