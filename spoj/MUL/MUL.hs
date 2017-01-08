{-# OPTIONS_GHC -O2 -optc-O2 #-}
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

solveCase :: BS.ByteString -> Integer
solveCase s =
  let
    Just (a, r) = BS.readInteger s
    Just (b, _) = BS.readInteger $ BS.dropWhile isSpace r
  in
    a*b

main = do
  (_:ls) <- BS.lines `fmap` BS.getContents
  mapM_ putStrLn $ map show $ map solveCase ls
