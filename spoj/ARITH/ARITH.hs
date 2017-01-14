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

showbs = BS.pack . show

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
    Line 0 (showbs a),
    Line 0 (BS.cons op (showbs b)),
    Dashes
  ]

subTotalLines expr@(Expr a '*' b) =
  if length subTotals == 2
  then []
  else subTotals
  where
    bLine = reverse $ map (\c -> read [c] :: Integer) $ show b
    subTotals = (map (\(n, s) -> Line n s) $ zip [0..] $ map (showbs . (* a)) bLine) ++ [ Dashes ]
subTotalLines _ = []

resultLines expr = [ Line 0 (showbs $ result expr) ]

format expr = alignRight (topLines expr ++ subTotalLines expr ++ resultLines expr)

solve s =
  case parseExpr s of
    Just expr -> format expr

main = BS.interact (BS.unlines . map solve . tail . BS.lines)
