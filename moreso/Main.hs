import Control.Arrow (first)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.List (intercalate)

data Form a = Sequence [Form a] | Atom a deriving (Eq)
newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol s) = s -- FIXME: Escape some characters
instance Read Symbol where
  readsPrec _ s
    | null name = []
    | otherwise = [(Symbol name, rest)]
    where
      isSym :: Char -> Bool
      isSym c = not (c `elem` "()") && not (isSpace c)

      name :: String
      name = takeWhile isSym . dropWhile isSpace $ s

      rest :: String
      rest = dropWhile isSym . dropWhile isSpace $ s

instance Show a => Show (Form a) where
  show (Atom a)    = show a
  show (Sequence xs) = "(" ++ intercalate " " (map show xs) ++ ")"
instance Read a => Read (Form a) where
  readsPrec n s = first Sequence <$> readParen True readSequence s <|>
                  first Atom <$> reads s
    where
      readSequence :: Read a => ReadS [a]
      readSequence s =
        case reads s of
          []     -> [([], s)]
          parses -> do (x, rest) <- parses
                       (xs, rest') <- readSequence rest
                       pure (x : xs, rest')

data Expr a = Reference Symbol
            | Forall Symbol a a
            | Apply a [a]

main :: IO ()
main = putStrLn "Hello, world!"
