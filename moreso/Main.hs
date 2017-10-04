{-# LANGUAGE DeriveFunctor #-}

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.Functor.Foldable (Fix(..), ana, cata)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP(..), choice, readP_to_S, readS_to_P,
                                     skipSpaces, many, munch1, char, between)

-- TODO:
-- * Escape control characters in symbols
-- * `unform` will do weird things in undefined cases

data Form a = Sequence [Form a] | Atom a deriving (Eq)
newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol s) = s
instance Read Symbol where
  readsPrec _ = readP_to_S $ fmap Symbol $ skipSpaces *> munch1 isSymChar
    where
      isSymChar :: Char -> Bool
      isSymChar c = not (c `elem` "()" || isSpace c)

instance Show a => Show (Form a) where
  show (Atom a)      = show a
  show (Sequence xs) = "(" ++ intercalate " " (map show xs) ++ ")"
instance Read a => Read (Form a) where
  readsPrec _ = readP_to_S $ choice [ Sequence <$> between lparen rparen (many readpa)
                                    , Atom <$> readS_to_P reads
                                    ]
    where
      lparen = skipSpaces *> char '('
      rparen = skipSpaces *> char ')'
      readpa = skipSpaces *> readS_to_P reads

type SExpr = Form Symbol

class SExprRepresentable f where
  fromSExpr :: SExpr -> f SExpr
  toSExpr :: f SExpr -> SExpr

data Pattern = PName Symbol
             | PList Symbol [Pattern]

data DefineClause a = DefineClause Symbol [Pattern] a
                      deriving (Functor)

data Expr a = Reference Symbol
            | Define a [DefineClause a]
            | Forall Symbol a a
            | Apply a [a]
            deriving (Functor)

instance SExprRepresentable Expr where
    fromSExpr (Atom sym@(Symbol _))                                              = Reference sym
    fromSExpr (Sequence (Atom (Symbol "define") : ty : clauses))                 = Define ty $ map clause clauses
      where
        clause :: SExpr -> DefineClause SExpr
        clause (Sequence [Sequence (Atom fname : args), expr]) = DefineClause fname (map pattern args) expr

        pattern                               :: SExpr -> Pattern
        pattern (Atom var)                    = PName var
        pattern (Sequence (Atom head : tail)) = PList head $ map pattern tail
    fromSExpr (Sequence [Atom (Symbol "Forall"), Atom var@(Symbol _), ty, expr]) = Forall var ty expr
    fromSExpr (Sequence (x : xs))                                                = Apply x xs

    toSExpr (Reference var)      = Atom var
    toSExpr (Define ty clauses)  = Sequence (Atom (Symbol "define") : ty : map clause clauses)
      where
        clause                     :: DefineClause SExpr -> SExpr
        clause (DefineClause fname args expr) = Sequence [Sequence (Atom fname : map pattern args), expr]

        pattern              :: Pattern -> SExpr
        pattern (PName name) = Atom name
        pattern (PList p ps) = Sequence $ Atom p : map pattern ps
    toSExpr (Forall var ty expr) = Sequence [Atom (Symbol "Forall"), Atom var, ty, expr]
    toSExpr (Apply x xs)         = Sequence (x : xs)

unform :: SExpr -> Fix Expr
unform = ana fromSExpr

form :: Fix Expr -> SExpr
form = cata toSExpr

main :: IO ()
main = putStrLn "Hello, world!"
