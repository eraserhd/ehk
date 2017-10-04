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

data Expr a = Reference Symbol
            | Forall Symbol a a
            | Apply a [a]
            deriving (Functor, Eq)

unform :: SExpr -> Fix Expr
unform = ana unform'
  where
    unform'                                                                    :: SExpr -> Expr SExpr
    unform' (Atom sym@(Symbol _))                                              = Reference sym
    unform' (Sequence [Atom (Symbol "Forall"), Atom var@(Symbol _), ty, expr]) = Forall var ty expr
    unform' (Sequence (x : xs))                                                = Apply x xs

form :: Fix Expr -> SExpr
form = cata form'
  where
    form'                      :: Expr SExpr -> SExpr
    form' (Reference var)      = Atom var
    form' (Forall var ty expr) = Sequence [Atom (Symbol "Forall"), Atom var, ty, expr]
    form' (Apply x xs)         = Sequence (x : xs)


main :: IO ()
main = putStrLn "Hello, world!"
