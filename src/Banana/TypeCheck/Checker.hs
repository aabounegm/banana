{-|
Module      : Checker
Description : Semantic Analyzer

Given the parsed AST, the Semantic Checker verifies that the input program
  is valid and transforms the AST into another intermediate representation
  for code generation.
-}
module Banana.TypeCheck.Checker where
