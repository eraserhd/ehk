{-# OPTIONS -O2 #-}

import Data.Array
import Data.Char (isSpace)
import Data.Either

data Expr = A | B | Turn Expr | Sew Expr Expr
     deriving (Show)

parse :: String -> [Expr]
parse "" = []
parse s = case parseExpr s of
            (e, ';' : r) -> e : (parse r)

parseExpr :: String -> (Expr, String)
parseExpr ('A' : cs) = (A, cs)
parseExpr ('B' : cs) = (B, cs)
parseExpr ('t' : 'u' : 'r' : 'n' : '(' : cs) =
    case parseExpr cs of 
        (e, ')' : rs) -> (Turn e, rs)
parseExpr ('s' : 'e' : 'w' : '(' : cs) =
    case parseExpr cs of
        (e1, ',' : rs) ->
            case parseExpr rs of
                (e2, ')' : r2) -> (Sew e1 e2, r2)

bottom :: Array (Int,Int) Char -> Int
bottom a = fst $ snd $ bounds a

right :: Array (Int,Int) Char -> Int
right a = snd $ snd $ bounds a

a :: Array (Int, Int) Char
a = array ((0,0),(1,1)) [
            ((0,0),'/'),
            ((0,1),'/'),
            ((1,0),'/'),
            ((1,1),'+')
         ]

b :: Array (Int,Int) Char
b = array ((0,0),(1,1)) [
            ((0,0),'-'),
            ((0,1),'-'),
            ((1,0),'-'),
            ((1,1),'-')
         ]

turnChar :: Char -> Char
turnChar '/' = '\\'
turnChar '-' = '|'
turnChar '\\' = '/'
turnChar '|' = '-'
turnChar '+' = '+'

turn :: Array (Int, Int) Char -> Array (Int, Int) Char
turn a = array ((0,0),(right a, bottom a)) $
                        map (\((i, j),c) -> ((j, (bottom a) - i), turnChar c)) $ assocs a

sew :: Array (Int, Int) Char -> Array (Int, Int) Char -> Array (Int, Int) Char
sew a b = array ((0,0),(bottom a, 1 + (right a) + (right b))) (
              (assocs a) ++ (map (\((i,j),c) -> ((i, j + 1 + (right a)), c)) $ assocs b)
              )

eval :: Expr -> Either () (Array (Int, Int) Char)
eval A = Right a
eval B = Right b
eval (Sew ae be) = case eval ae of
                     Left () -> Left ()
                     Right a ->
                        case eval be of
                            Left () -> Left ()
                            Right b ->
                                if (fst $ snd $ bounds a) /= (fst $ snd $ bounds b)
                                then Left ()
                                else Right $ sew a b
eval (Turn ae) = case eval ae of
                    Left () -> Left ()
                    Right a -> Right $ turn a

formatArr :: Array (Int, Int) Char -> String
formatArr a = unlines $ [ [ a ! (i,j) | j <- [0..(right a)] ] | i <- [0..(bottom a)] ]

format :: (Int, Either () (Array (Int, Int) Char)) -> String
format (n, a) = "Quilt " ++ (show n) ++ ":\n" ++
                case a of
                    Left () -> "error\n"
                    Right a -> formatArr a

main = interact (concat . map format . zip [1..] . map eval . parse . filter (not . isSpace))

-- vi:set sts=4 sw=4 ai et:
