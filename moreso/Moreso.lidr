
> %default total

## S-Expressions

Our lisp's fundamental data type isn't {\em quite} an S-expression--we borrow
Haskell's list type instead of making our own pairs.  This allows us to avoid
implementing dotted pair notation.

> data SExpression : Type where
>   Symbol   : String -> SExpression
>   Sequence : List SExpression -> SExpression

S-expressions are printed lisp-style using `write`.

> write : SExpression -> String
> write (Symbol s)    = s
> write (Sequence xs) = "(" ++ unwords (elements xs) ++ ")"
>   where
>     elements : List SExpression -> List String
>     elements []        = []
>     elements (x :: xs) = write x :: elements xs
