{-|
Module      : Checker
Description : Semantic Analyzer

Given the parsed AST, the Semantic Checker verifies that the input program
  is valid and transforms the AST into another intermediate representation
  for code generation.
-}
module Banana.TypeCheck.Checker where

import           Banana.Syntax.AST
import           Data.List         (find, nub)
import           Data.Maybe        (isJust)

-- TODO: Change the Bools into `Either e (Program _)` for easier chaining
--  and error message propagation

-- | Checks if a given expression is a valid LHS in an assignment
validLHS :: Expr a -> Bool
validLHS (Var _) = True
validLHS _       = False

-- | Looks up the variable declaration that declares a given variable
varToReference :: (Eq a) => Expr a -> [VarDecl a] -> Maybe (VarDecl a)
varToReference (Var name) = find ((==name) . varName)
varToReference _          = const Nothing

-- | Checks if all assignments are valid
assignmentsAreValid :: Program a -> Bool
assignmentsAreValid = all (validLHS . varAssignee) . assignments

varDeclsAreUnique :: (Eq a) => Program a -> Bool
varDeclsAreUnique p = length decls == length (nub decls)
  where
    decls = map varName $ varDecls p

allVarsDeclared :: (Eq a) => Program a -> Bool
allVarsDeclared prog = all
  (isJust . (`varToReference` varsTable))
  (concatMap expressions (assignments prog))
  where
    vars (Add l r) = vars l ++ vars r
    vars (Sub l r) = vars l ++ vars r
    vars (Mul l r) = vars l ++ vars r
    vars (Div l r) = vars l ++ vars r
    vars (Var a)   = [Var a]
    vars (Lit _)   = []
    vars (Not e)   = vars e
    vars (Or  l r) = vars l ++ vars r
    vars (And l r) = vars l ++ vars r
    vars (Eq  l r) = vars l ++ vars r
    vars (NEq l r) = vars l ++ vars r
    vars (LEq l r) = vars l ++ vars r
    vars (MEq l r) = vars l ++ vars r
    vars (Less l r) = vars l ++ vars r
    vars (More l r) = vars l ++ vars r
    vars (FuncCallExpr (FuncCall _ args)) = concatMap vars args

    expressions (VarAssign lhs rhs) = vars lhs ++ vars rhs

    varsTable = varDecls prog

checkAll :: (Eq a) => Program a -> Bool
checkAll = and . sequence
  [ allVarsDeclared
  , assignmentsAreValid
  , varDeclsAreUnique
  ]

-- linkVarToDecl :: Program String -> Program (VarDecl String)
-- linkVarToDecl = _
