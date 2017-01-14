import GHC.Int ( Int64 )
import qualified Data.ByteString.Lazy.Char8 as BS

data Line = Line Int64 BS.ByteString | Dashes
data Expr = Expr Integer Char Integer

result (Expr a '+' b) = a + b
result (Expr a '-' b) = a - b
result (Expr a '*' b) = a * b

readExpr :: BS.ByteString -> Maybe Expr
readExpr s = do
  (a, s) <- BS.readInteger s
  (op, s) <- BS.uncons s
  (b, _) <- BS.readInteger s
  return $ Expr a op b

showbs :: Show a => a -> BS.ByteString
showbs = BS.pack . show

nl :: BS.ByteString
nl = BS.singleton '\n'

formatLines :: [Line] -> BS.ByteString
formatLines ls = BS.concat $ fmt ls
           where
             lineWidth (Line n bs) = n + BS.length bs
             lineWidth Dashes      = 0

             w = maximum $ map lineWidth ls
             padWidth line@(Line n bs) = w - (BS.length bs) - n
             padding line = BS.replicate (padWidth line) ' '
             padded line@(Line _ bs) = BS.concat [ padding line, bs, nl ]

             fmt [] = []
             fmt (l1@(Line _ _) : Dashes : l2@(Line _ _) : ls) =
               let width = max (lineWidth l1) (lineWidth l2)
               in padded l1 : padded (Line 0 (BS.replicate width '-')) : fmt (l2 : ls)
             fmt (l : ls) = padded l : fmt ls

topLines :: Expr -> [Line]
topLines (Expr a op b) = [
    Line 0 (showbs a),
    Line 0 (BS.cons op (showbs b)),
    Dashes
  ]

middleLines :: Expr -> [Line]
middleLines expr@(Expr a op b)
  | op /= '*' = []
  | b < 10    = []
  | otherwise = subTotals ++ [ Dashes ]
    where
      bDigits :: [Integer]
      bDigits = reverse $ map (read . return) $ show b

      intermediateProducts :: [Integer]
      intermediateProducts = map (* a) bDigits

      subTotals :: [Line]
      subTotals = map (\(indent, n) -> Line indent $ showbs n) $ zip [0..] intermediateProducts

resultLines :: Expr -> [Line]
resultLines expr = [ Line 0 $ showbs $ result expr ]

format :: Expr -> BS.ByteString
format expr = formatLines $ topLines expr ++ middleLines expr ++ resultLines expr

solve :: BS.ByteString -> BS.ByteString
solve s = let Just expr = readExpr s
          in format expr

main :: IO ()
main = BS.interact $ BS.unlines . map solve . tail . BS.lines
