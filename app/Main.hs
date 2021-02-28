module Main where

import           Banana.Syntax.Parser
import           System.Environment   (getArgs)
import           Text.Trifecta        (parseFromFile, parseString)

repl :: IO ()
repl = do
  line <- getLine
  case line of
    ""  -> return ()
    str -> do
      print $ parseString parseProgram mempty (str ++ "\n")
      repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    [file] -> parseFromFile parseProgram file >>= print
    _      -> print "Usage: banana [file path]"

