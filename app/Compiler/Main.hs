module Main where

import           Banana.Syntax.Parser
import           Banana.TypeCheck.Checker
import           System.Environment       (getArgs)
import           Text.Trifecta            (parseFromFile, parseString)

repl :: IO ()
repl = do
  line <- getLine
  case line of
    ""  -> return ()
    str -> do
      print $ parseString parseProgram mempty (str ++ "\n")
      repl

compile :: String -> IO String
compile file = do
  ast <- parseFromFile parseProgram file
  let validity = fmap checkAll ast
  case validity of
    Nothing    -> return "Failed to parse input"
    Just False -> return "Semantic checking failed"
    Just True  -> return (show ast)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    [file] -> compile file >>= print
    _      -> print "Usage: banana [file path]"

