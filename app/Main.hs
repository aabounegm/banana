module Main where

import           Banana.Syntax.Parser
import           Text.Trifecta        (parseString)

main :: IO ()
main = do
  line <- getLine
  case line of
    ""  -> return ()
    str -> do
      print $ parseString parseExpr mempty (str ++ "\n")
      main
