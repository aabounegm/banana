{-|
Module      : AST
Description : Abstract Syntax Tree types

This module holds the data types that are uses to represent the parsed program.
-}
module Banana.Syntax.AST where

-- | The possible types in the Banana language
data Type = Number
          | Array Integer Type
          deriving (Show, Eq)

-- | A variable declaration of the form "var x: num"
data VarDecl = VarDecl { varName :: String
                       , varType :: Type
                       } deriving (Show, Eq)

-- | An arithmetic expression
data Expr a = Add (Expr a) (Expr a) -- ^ Addition
            | Sub (Expr a) (Expr a) -- ^ Subtraction
            | Mul (Expr a) (Expr a) -- ^ Multiplication
            | Div (Expr a) (Expr a) -- ^ Division
            | Var a                 -- ^ Variable (identifier)
            | Lit Double            -- ^ A literal value
            deriving (Show, Eq)

-- | A variable assignment of the form "x := y"
data VarAssign a = VarAssign
  { varAssignee :: Expr a -- ^ LHS
  , varExpr     :: Expr a -- ^ RHS
  } deriving (Show, Eq)
