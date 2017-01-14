module Main where
import Data.List ( isSuffixOf )
import Data.Algorithm.DiffContext ( getContextDiff, prettyContextDiff )
import System.Directory ( listDirectory )
import System.Process ( callCommand, readProcess )
import Text.PrettyPrint ( text )

isSourceFile name = ".hs" `isSuffixOf` name
isInputFile name = ".in" `isSuffixOf` name

sourceFile = head . filter isSourceFile

inputFiles = filter isInputFile

basename [] = []
basename ('.' : cs) = []
basename (c : cs) = c : basename cs

outputFile f = basename f ++ ".out"

checkInput :: FilePath -> FilePath -> IO ()
checkInput exe inputFile = do
  putStr (inputFile ++ "... ")
  input <- readFile inputFile
  output <- readProcess exe [] input
  expected <- readFile (outputFile inputFile)
  if output == expected
  then putStrLn "ok"
  else do
    putStrLn "error"
    let diff = getContextDiff 3 (lines output) (lines expected)
    let p = prettyContextDiff (text inputFile) (text $ outputFile inputFile) text diff
    putStrLn $ show p

main :: IO ()
main = do files <- listDirectory "."
          let src = sourceFile files
          callCommand $ "ghc " ++ src
          let exe = "./" ++ basename src
          mapM_ (checkInput exe) $ inputFiles files
