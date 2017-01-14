import qualified Data.ByteString.Lazy.Char8 as BS

parse s = do
  (a, s) <- BS.readInteger s
  (op, s) <- BS.uncons s
  (b, _) <- BS.readInteger s
  return (a, op, b)

alignRight ls = BS.concat $ map pad ls
           where
             w = maximum $ map (\( n, bs ) -> n + BS.length bs) ls
             pad ( n, bs ) =
               BS.concat [
                 BS.replicate (w - (BS.length bs) - n) ' ',
                 bs,
                 BS.singleton '\n'
               ]

format a op b | op == '-' || op == '+' =
  alignRight [
    ( 0, as ),
    ( 0, BS.cons op bs ),
    ( 0, BS.replicate dashes '-' ),
    ( 0, cs )
  ]
  where
    as = BS.pack $ show a
    bs = BS.pack $ show b
    cs = BS.pack $ show $ if op == '+'
                          then a + b
                          else a - b
    dashes = maximum [1 + BS.length bs, BS.length cs]
format a '*' b =
  alignRight ([
    ( 0, as ),
    ( 0, BS.cons '*' bs ),
    ( 0, BS.replicate dashes1 '-' )
  ] ++ subTotals ++ finalTotal)
  where
    as = BS.pack $ show a
    bs = BS.pack $ show b
    bDigits = reverse $ map (\c -> read [c] :: Integer) $ show b
    subTotals = zip [0..] $ map (BS.pack . show . (* a)) bDigits
    bLastDigit = head bDigits
    cs = BS.pack $ show (bLastDigit * a)
    dashes1 = maximum [1 + BS.length bs, BS.length cs]
    finalTotal = if length bDigits == 1
                 then []
                 else let result = BS.pack $ show $ a * b
                      in [
                        ( 0, BS.replicate (BS.length result) '-' ),
                        ( 0, result )
                      ]

solve s =
  case parse s of
    Just (a, op, b) -> format a op b

main = BS.interact (BS.unlines . map solve . tail . BS.lines)
