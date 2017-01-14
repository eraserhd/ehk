import GHC.Int ( Int64 )
import qualified Data.ByteString.Lazy.Char8 as BS

data Line = Line Int64 BS.ByteString | Dashes
data Expr = Expr Integer Char Integer

result (Expr a '+' b) = a + b
result (Expr a '-' b) = a - b
result (Expr a '*' b) = a * b

parseExpr s = do
  (a, s) <- BS.readInteger s
  (op, s) <- BS.uncons s
  (b, _) <- BS.readInteger s
  return $ Expr a op b

alignRight ls = BS.concat $ pad ls
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

topLines (Expr a op b) = [
    Line 0 as,
    Line 0 (BS.cons op bs),
    Dashes
  ]
  where
    as = BS.pack $ show a
    bs = BS.pack $ show b

format expr@(Expr a op b) | op == '-' || op == '+' =
  alignRight (topLines expr ++ [Line 0 cs])
  where
    cs = BS.pack $ show $ result expr
format expr@(Expr a op@'*' b) =
  alignRight (topLines expr ++ subTotals ++ finalTotal)
  where
    bLine = reverse $ map (\c -> read [c] :: Integer) $ show b
    subTotals = map (\(n, s) -> Line n s) $ zip [0..] $ map (BS.pack . show . (* a)) bLine
    bLastDigit = head bLine
    cs = BS.pack $ show (bLastDigit * a)
    finalTotal = if length bLine == 1
                 then []
                 else let resultText = BS.pack $ show $ result expr
                      in [
                        Line 0 (BS.replicate (BS.length resultText) '-'),
                        Line 0 resultText
                      ]

solve s =
  case parseExpr s of
    Just expr -> format expr

main = BS.interact (BS.unlines . map solve . tail . BS.lines)
