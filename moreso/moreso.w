\input supp-pdf

\def\title{Moreso}
\def\topofcontents{\null\vfill
  \centerline{\titlefont Moreso}
  \vskip 15pt
  \vfill}
\def\botofcontents{\vfill
%...
}

\def\N#1#2#3.{% beginning of starred section
\section{#3}
}

\documentclass{article}
\begin{document}

@* The Language.

\begin{verbatim}
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
\end{verbatim}

\def\istype{\mbox{ is a type}}

\begin{displaymath}
\frac{}{\Gamma\vdash\mbox{{\tt Type}}\istype}
\end{displaymath}

\begin{displaymath}
\frac{
\Gamma\vdash A\istype
\quad
\Gamma,x:A\vdash B\istype
}{
\Gamma\vdash\mbox{\tt (Forall } x\ A\ B\mbox{\tt)}\istype}
\end{displaymath}

\begin{displaymath}
\frac{
  ??
}{
\Gamma\vdash\mbox{\tt (Inductive} \mbox{\tt)}\istype
}
\end{displaymath}

\begin{displaymath}
\frac{
\Gamma\vdash A\istype\quad
\Gamma,x:A\vdash B\istype\quad
\Gamma,x:A\vdash e : B
}{
\Gamma\vdash\mbox{\tt (lambda } x\ e\mbox{\tt)} : \mbox{\tt (Forall } x\ A\ B\mbox{\tt)}
}
\end{displaymath}

@

\end{document}
