\documentclass{article}
\usepackage{minted}
\newminted[code]{haskell}{}
\newminted[moreso]{scheme}{}
\begin{document}

\section{The Language}
\begin{moreso}
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
\end{moreso}

\section{Preamble}
\begin{code}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances #-}

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.Functor.Foldable (Fix(..), ana, cata)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP(..), (<++), readP_to_S, readS_to_P,
                                     skipSpaces, many, munch1, char, between)
\end{code}

\section{S-Expressions}

Our lisp's fundamental data type isn't {\em quite} an S-expression--we borrow
Haskell's list type instead of making our own pairs.  This allows us to avoid
implementing dotted pair notation.

\begin{code}
data SExpression = Symbol String
                 | Sequence [SExpression]
\end{code}

\subsection{Textual Representation}
S-expressions are printed and parsed using lisp's representation, rather than
Haskell's:
\begin{code}
instance Show SExpression where
  show (Symbol name) = name
  show (Sequence xs) = "(" ++ intercalate " " (show <$> xs) ++ ")"

instance Read SExpression where
  readsPrec _ = readP_to_S sExpression
    where
      sExpression   = skipSpaces *> (sequence <++ symbol)
      sequence      = Sequence <$> parenthesized (many sExpression)
      symbol        = Symbol <$> munch1 isSymbolChar
      parenthesized = between leftParen rightParen
      leftParen     = char '('
      rightParen    = skipSpaces *> char ')'

      isSymbolChar :: Char -> Bool
      isSymbolChar c = not (c `elem` "()" || isSpace c)
\end{code}

\subsection{Value Conversion}
A type {\em a} is {\em S-expressable} if we can convert values to
S-expressions and back.

{\bf FIXME:} I'm not too happy about these type signatures.  Are there other
recursive data structures we'll want to convert to S-expressions?  Perhaps
we just want a constrained instance for functors in terms of {\tt ana} and
{\tt cata}?  I think so.

\begin{code}
class SExpressable a where
  interpretLayer :: SExpression -> a
  expressLayer   :: a -> SExpression

interpret :: (Functor f, SExpressable (f SExpression)) =>
             SExpression -> Fix f
interpret = ana interpretLayer

express :: (Functor f, SExpressable (f SExpression)) =>
           Fix f -> SExpression
express = cata expressLayer
\end{code}

\section{Patterns}
\begin{code}
data Pattern = PName String
             | PList String [Pattern]

instance SExpressable Pattern where
  interpretLayer (Symbol var)                    = PName var
  interpretLayer (Sequence (Symbol head : tail)) = PList head $ map interpretLayer tail

  expressLayer (PName name) = Symbol name
  expressLayer (PList p ps) = Sequence $ Symbol p : map expressLayer ps
\end{code}


\section{...}
\begin{code}
data DefineClause a = DefineClause String [Pattern] a
                      deriving (Functor, Foldable, Traversable)

instance SExpressable (DefineClause SExpression) where
  interpretLayer (Sequence [Sequence (Symbol fname : args), expr]) = DefineClause fname (map interpretLayer args) expr

  expressLayer (DefineClause fname args expr) = Sequence [Sequence (Symbol fname : map expressLayer args), expr]

data Expr a = Reference String
            | Define a [DefineClause a]
            | Forall String a a
            | Apply a [a]
            deriving (Functor, Foldable, Traversable)

data DataConstructorDefinition a =
  DataConstructorDefinition String (Expr a)
  deriving (Functor, Foldable, Traversable)

instance SExpressable (DataConstructorDefinition SExpression) where
  interpretLayer (Sequence [(Symbol ctorName), ty])    = DataConstructorDefinition ctorName $ interpretLayer ty
  expressLayer (DataConstructorDefinition ctorName ty) = Sequence [Symbol ctorName, expressLayer ty]

data TypeDefinition a =
  TypeDefinition String (Expr a) [DataConstructorDefinition a]
  deriving (Functor, Foldable, Traversable)

instance SExpressable (TypeDefinition SExpression) where
  interpretLayer (Sequence (Symbol "type" : Symbol typeCtor : ty : dataCtors)) =
    TypeDefinition typeCtor (interpretLayer ty) $ map interpretLayer dataCtors

  expressLayer (TypeDefinition typeCtor ty dataCtors) =
    Sequence $ Symbol "type" : Symbol typeCtor : expressLayer ty : map expressLayer dataCtors

instance SExpressable (Expr SExpression) where
  interpretLayer (Symbol var)                                       = Reference var
  interpretLayer (Sequence (Symbol "define" : ty : clauses))        = Define ty $ map interpretLayer clauses
  interpretLayer (Sequence [Symbol "Forall", Symbol var, ty, expr]) = Forall var ty expr
  interpretLayer (Sequence (x : xs))                                = Apply x xs

  expressLayer (Reference var)      = Symbol var
  expressLayer (Define ty clauses)  = Sequence (Symbol "define" : ty : map expressLayer clauses)
  expressLayer (Forall var ty expr) = Sequence [Symbol "Forall", Symbol var, ty, expr]
  expressLayer (Apply x xs)         = Sequence (x : xs)

main :: IO ()
main = putStrLn "Hello, world!"
\end{code}
\end{document}
