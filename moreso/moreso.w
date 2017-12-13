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

expr           := 'Type'                                 ; Root type
               |  ('Forall' symbol Ty expr)              ; Dependent types
               |  is-expr
               |  lambda-expr
               |  introduce-expr
               |  eliminate-expr
               |  type-expr
               |  symbol                                 ; variable reference
               |  (expr_1 ... expr_n)                    ; application
\end{verbatim}

\begin{displaymath}
\frac{Hello}{World}(IS)
\end{displaymath}

@

\end{document}
