import Data.List (intercalate)

data Tree a = Sequence [a] | Single a deriving (Eq)
newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol s) = s -- FIXME: Escape some characters

instance Show a => Show (Tree a) where
  show (Single a)    = show a
  show (Sequence xs) = "(" ++ intercalate " " (map show xs) ++ ")"

data Expr a = Reference Symbol
            | Forall Symbol a a
            | Apply a [a]

main :: IO ()
main = putStrLn "Hello, world!"
