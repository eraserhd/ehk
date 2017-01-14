import GHC.Int ( Int64 )
import qualified Data.ByteString.Lazy.Char8 as BS

data Line = Line Int64 BS.ByteString

parse s = do
  (a, s) <- BS.readInteger s
  (op, s) <- BS.uncons s
  (b, _) <- BS.readInteger s
  return (a, op, b)

alignRight ls = BS.concat $ map pad ls
           where
             lineWidth (Line n bs) = n + BS.length bs
             w = maximum $ map lineWidth ls
             pad (Line n bs) =
               BS.concat [
                 BS.replicate (w - (BS.length bs) - n) ' ',
                 bs,
                 BS.singleton '\n'
               ]

format a op b | op == '-' || op == '+' =
  alignRight [
    Line 0 as,
    Line 0 (BS.cons op bs),
    Line 0 (BS.replicate dashes '-'),
    Line 0 cs
  ]
  where
    as = BS.pack $ show a
    bs = BS.pack $ show b
    cs = BS.pack $ show $ if op == '+'
                          then a + b
                          else a - b
    dashes = maximum [1 + BS.length bs, BS.length cs]
format a op@'*' b =
  alignRight ([
    Line 0 as,
    Line 0 (BS.cons op bs),
    Line 0 (BS.replicate dashes1 '-')
  ] ++ subTotals ++ finalTotal)
  where
    as = BS.pack $ show a
    bs = BS.pack $ show b
    bLine = reverse $ map (\c -> read [c] :: Integer) $ show b
    subTotals = map (\(n, s) -> Line n s) $ zip [0..] $ map (BS.pack . show . (* a)) bLine
    bLastDigit = head bLine
    cs = BS.pack $ show (bLastDigit * a)
    dashes1 = maximum [1 + BS.length bs, BS.length cs]
    finalTotal = if length bLine == 1
                 then []
                 else let result = BS.pack $ show $ a * b
                      in [
                        Line 0 (BS.replicate (BS.length result) '-'),
                        Line 0 result
                      ]

solve s =
  case parse s of
    Just (a, op, b) -> format a op b

main = BS.interact (BS.unlines . map solve . tail . BS.lines)
