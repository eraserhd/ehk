{-# LANGUAGE DeriveFunctor #-}

import Control.Arrow (first)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Functor.Classes (Show1(..))
import Data.Functor.Foldable (Fix(..), unfix, ana, cata)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (readP_to_S, skipSpaces, munch1, satisfy)

-- TODO:
-- * Escape control characters in symbols
-- * `parse` will do weird things in undefined cases

data Form a = Sequence [Form a] | Atom a deriving (Eq)
newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol s) = s
instance Read Symbol where
  readsPrec _ s = first Symbol <$> readP_to_S (skipSpaces *> munch1 (not . notSymChar)) s
    where
      notSymChar :: Char -> Bool
      notSymChar c = c `elem` "()" || isSpace c

instance Show a => Show (Form a) where
  show (Atom a)    = show a
  show (Sequence xs) = "(" ++ intercalate " " (map show xs) ++ ")"
instance Read a => Read (Form a) where
  readsPrec _ s = first Sequence <$> readParen True readSequence s <|>
                  first Atom <$> reads s
    where
      readSequence   :: Read a => ReadS [a]
      readSequence s =
        case reads s of
          []     -> [([], s)]
          parses -> do (x, rest) <- parses
                       first (x :) <$> readSequence rest

type SExpr = Form Symbol

data Expr a = Reference Symbol
            | Forall Symbol a a
            | Apply a [a]
            deriving (Functor, Eq)
instance Show1 Expr where
  liftShowsPrec sPrec _ n (Reference x)      = shows x
  liftShowsPrec sPrec _ n (Forall x ty expr) =
    (++ "(Forall " ++ shows x " " ++ sPrec n ty " " ++ sPrec n expr ")")
  liftShowsPrec sPrec _ n (Apply x xs)       =
    (++ "(" ++ sPrec n x (showSequence xs))
    where
      showSequence []       = ")"
      showSequence (x : xs) = " " ++ sPrec n x (showSequence xs)

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
