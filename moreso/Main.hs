{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, TypeOperators #-}

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.Functor.Foldable (Fix(..), ana, cata)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP(..), (<++), readP_to_S, readS_to_P,
                                     skipSpaces, many, munch1, char, between)
import GHC.Generics ((:+:)(..))

-- TODO:
-- * Escape control characters in symbols
-- * `unform` should use gana, MonadFail for errors
-- * Rename `define` to `value`?

data SExpr = Symbol String
           | Sequence [SExpr]
           deriving (Eq)

instance Show SExpr where
  show (Symbol a)    = a
  show (Sequence xs) = "(" ++ intercalate " " (map show xs) ++ ")"
instance Read SExpr where
  readsPrec _ = readP_to_S grammar
    where
      grammar  = skipSpaces *> (sequence <++ symbol)
      sequence = Sequence <$> between (char '(') (skipSpaces *> char ')') (many grammar)
      symbol   = Symbol <$> munch1 isSymChar

      isSymChar :: Char -> Bool
      isSymChar c = not (c `elem` "()" || isSpace c)

class SExpressable f where
  fromSExpr :: SExpr -> f SExpr
  toSExpr   :: f SExpr -> SExpr

data Pattern a = PName String
               | PList String [Pattern a]
               deriving (Functor, Foldable, Traversable)

instance SExpressable Pattern where
  fromSExpr (Symbol var)                  = PName var
  fromSExpr (Sequence (Symbol head : tail)) = PList head $ map fromSExpr tail

  toSExpr (PName name) = Symbol name
  toSExpr (PList p ps) = Sequence $ Symbol p : map toSExpr ps


data DefineClause a = DefineClause String [Pattern a] a
                      deriving (Functor, Foldable, Traversable)

instance SExpressable DefineClause where
  fromSExpr (Sequence [Sequence (Symbol fname : args), expr]) = DefineClause fname (map fromSExpr args) expr

  toSExpr (DefineClause fname args expr) = Sequence [Sequence (Symbol fname : map toSExpr args), expr]

data Expr a = Reference String
            | Define a [DefineClause a]
            | Forall String a a
            | Apply a [a]
            deriving (Functor, Foldable, Traversable)

data DataConstructorDefinition a =
  DataConstructorDefinition String (Expr a)
  deriving (Functor, Foldable, Traversable)

instance SExpressable DataConstructorDefinition where
  fromSExpr (Sequence [(Symbol ctorName), ty])    = DataConstructorDefinition ctorName $ fromSExpr ty
  toSExpr (DataConstructorDefinition ctorName ty) = Sequence [Symbol ctorName, toSExpr ty]

data TypeDefinition a =
  TypeDefinition String (Expr a) [DataConstructorDefinition a]
  deriving (Functor, Foldable, Traversable)

instance SExpressable TypeDefinition where
  fromSExpr (Sequence (Symbol "type" : Symbol typeCtor : ty : dataCtors)) =
    TypeDefinition typeCtor (fromSExpr ty) $ map fromSExpr dataCtors

  toSExpr (TypeDefinition typeCtor ty dataCtors) =
    Sequence $ Symbol "type" : Symbol typeCtor : toSExpr ty : map toSExpr dataCtors

data TopLevel a = TypeDefinition a :+: Expr a

instance SExpressable Expr where
  fromSExpr (Symbol var)                                       = Reference var
  fromSExpr (Sequence (Symbol "define" : ty : clauses))        = Define ty $ map fromSExpr clauses
  fromSExpr (Sequence [Symbol "Forall", Symbol var, ty, expr]) = Forall var ty expr
  fromSExpr (Sequence (x : xs))                                = Apply x xs

  toSExpr (Reference var)      = Symbol var
  toSExpr (Define ty clauses)  = Sequence (Symbol "define" : ty : map toSExpr clauses)
  toSExpr (Forall var ty expr) = Sequence [Symbol "Forall", Symbol var, ty, expr]
  toSExpr (Apply x xs)         = Sequence (x : xs)

unform :: (SExpressable a, Functor a) => SExpr -> Fix a
unform = ana fromSExpr

form :: (SExpressable a, Functor a) => Fix a -> SExpr
form = cata toSExpr

main :: IO ()
main = putStrLn "Hello, world!"
