{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, TypeOperators #-}

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.Functor.Foldable (Fix(..), ana, cata)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP(..), choice, readP_to_S, readS_to_P,
                                     skipSpaces, many, munch1, char, between)
import GHC.Generics ((:+:)(..))

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

data Pattern a = PName Symbol
               | PList Symbol [Pattern a]
               deriving (Functor, Foldable, Traversable)

instance SExprRepresentable Pattern where
  fromSExpr (Atom var)                    = PName var
  fromSExpr (Sequence (Atom head : tail)) = PList head $ map fromSExpr tail

  toSExpr (PName name) = Atom name
  toSExpr (PList p ps) = Sequence $ Atom p : map toSExpr ps


data DefineClause a = DefineClause Symbol [Pattern a] a
                      deriving (Functor, Foldable, Traversable)

instance SExprRepresentable DefineClause where
  fromSExpr (Sequence [Sequence (Atom fname : args), expr]) = DefineClause fname (map fromSExpr args) expr

  toSExpr (DefineClause fname args expr) = Sequence [Sequence (Atom fname : map toSExpr args), expr]

data Expr a = Reference Symbol
            | Define a [DefineClause a]
            | Forall Symbol a a
            | Apply a [a]
            deriving (Functor, Foldable, Traversable)

data DataConstructorDefinition a =
  DataConstructorDefinition Symbol (Expr a)
  deriving (Functor, Foldable, Traversable)

instance SExprRepresentable DataConstructorDefinition where
  fromSExpr (Sequence [Atom sym@(Symbol _), ty]) = DataConstructorDefinition sym $ fromSExpr ty
  toSExpr (DataConstructorDefinition sym ty)     = Sequence [Atom sym, toSExpr ty]

data TypeDefinition a =
  TypeDefinition Symbol (Expr a) [DataConstructorDefinition a]
  deriving (Functor, Foldable, Traversable)

instance SExprRepresentable TypeDefinition where
  fromSExpr (Sequence (Atom (Symbol "type") : Atom typeCtor@(Symbol _) : ty : dataCtors)) =
    TypeDefinition typeCtor (fromSExpr ty) $ map fromSExpr dataCtors

  toSExpr (TypeDefinition typeCtor ty dataCtors) =
    Sequence $ Atom (Symbol "type") : Atom typeCtor : toSExpr ty : map toSExpr dataCtors

data TopLevel a = TypeDefinition a :+: Expr a

instance SExprRepresentable Expr where
  fromSExpr (Atom sym@(Symbol _))                                              = Reference sym
  fromSExpr (Sequence (Atom (Symbol "define") : ty : clauses))                 = Define ty $ map fromSExpr clauses
  fromSExpr (Sequence [Atom (Symbol "Forall"), Atom var@(Symbol _), ty, expr]) = Forall var ty expr
  fromSExpr (Sequence (x : xs))                                                = Apply x xs

  toSExpr (Reference var)      = Atom var
  toSExpr (Define ty clauses)  = Sequence (Atom (Symbol "define") : ty : map toSExpr clauses)
  toSExpr (Forall var ty expr) = Sequence [Atom (Symbol "Forall"), Atom var, ty, expr]
  toSExpr (Apply x xs)         = Sequence (x : xs)

unform :: (SExprRepresentable a, Functor a) => SExpr -> Fix a
unform = ana fromSExpr

form :: (SExprRepresentable a, Functor a) => Fix a -> SExpr
form = cata toSExpr

main :: IO ()
main = putStrLn "Hello, world!"
