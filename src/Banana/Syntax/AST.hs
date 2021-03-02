{-|
Module      : AST
Description : Abstract Syntax Tree types

This module holds the data types that are uses to represent the parsed program.
-}
module Banana.Syntax.AST where

-- | The possible types in the Banana language
data Type = Number
          | Array Integer Type
          | Void
          deriving (Show, Eq)

-- | A variable declaration of the form "var x: num"
data VarDecl a = VarDecl
  { varName :: a
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

-- | A wrapper to collect all statement types together
data Statement a
  = VarDeclaration (VarDecl a)
  | VarAssignment  (VarAssign a)
  deriving (Show, Eq)

-- | A function definition
data Function a = Function
  { params     :: [VarDecl a]
  , returnType :: Type
  , body       :: [Statement a]
  } deriving (Show, Eq)

-- | The AST node representing the entire program with all of its structures
data Program a = Program
  { varDecls    :: [VarDecl]
  , assignments :: [VarAssign a]
  }  deriving (Show, Eq)
