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

formatLines :: [Line] -> BS.ByteString
formatLines ls = BS.concat $ pad ls
           where
             lineWidth (Line n bs) = n + BS.length bs
             lineWidth Dashes = 0

             w = maximum $ map lineWidth ls

             padLine (Line n bs) =
               BS.concat [
                 BS.replicate (w - (BS.length bs) - n) ' ',
                 bs,
                 BS.singleton '\n'
               ]

             pad [] = []
             pad (l1@(Line _ _) : Dashes : l2@(Line _ _) : ls) =
               let width = max (lineWidth l1) (lineWidth l2)
               in padLine l1 : padLine (Line 0 (BS.replicate width '-')) : pad (l2 : ls)
             pad (l : ls) = padLine l : pad ls

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
