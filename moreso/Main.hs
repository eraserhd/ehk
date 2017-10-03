import Control.Arrow (first)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.List (intercalate)

-- TODO: Escape control characters in symbols

data Form a = Sequence [Form a] | Atom a deriving (Eq)
newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol s) = s
instance Read Symbol where
  readsPrec _ s
    | null name = []
    | otherwise = [(Symbol name, rest)]
    where
      notSymChar :: Char -> Bool
      notSymChar c = c `elem` "()" || isSpace c

      name, rest :: String
      (name, rest) = break notSymChar . dropWhile isSpace $ s

instance Show a => Show (Form a) where
  show (Atom a)    = show a
  show (Sequence xs) = "(" ++ intercalate " " (map show xs) ++ ")"
instance Read a => Read (Form a) where
  readsPrec _ s = first Sequence <$> readParen True readSequence s <|>
                  first Atom <$> reads s
    where
      readSequence :: Read a => ReadS [a]
      readSequence s =
        case reads s of
          []     -> [([], s)]
          parses -> do (x, rest) <- parses
                       first (x :) <$> readSequence rest

data Expr a = Reference Symbol
            | Forall Symbol a a
            | Apply a [a]

main :: IO ()
main = putStrLn "Hello, world!"
