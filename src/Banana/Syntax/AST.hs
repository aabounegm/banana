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
data Expr a = Add   (Expr a) (Expr a)  -- ^ Addition
            | Sub   (Expr a) (Expr a)  -- ^ Subtraction
            | Mul   (Expr a) (Expr a)  -- ^ Multiplication
            | Div   (Expr a) (Expr a)  -- ^ Division
            | Var   a                  -- ^ Variable (identifier)
            | Lit   Double             -- ^ A literal value
            | Not   (Expr a)           -- ^ Unary not
            | Or    (Expr a) (Expr a)  -- ^ Binary or
            | And   (Expr a) (Expr a)  -- ^ Binary and
            | Less  (Expr a) (Expr a)  -- ^ Binary less
            | More  (Expr a) (Expr a)  -- ^ Binary more
            | LEq   (Expr a) (Expr a)  -- ^ Binary less or equal
            | MEq   (Expr a) (Expr a)  -- ^ Binary more or equal
            | Eq    (Expr a) (Expr a)  -- ^ Binary equal
            | NotEq (Expr a) (Expr a)  -- ^ Binary not equal
            deriving (Show, Eq)

-- | A variable assignment of the form "x := y"
data VarAssign a = VarAssign
  { varAssignee :: Expr a -- ^ LHS
  , varExpr     :: Expr a -- ^ RHS
  } deriving (Show, Eq)

-- | A definition of if statement
data IfStatement a = If
  { condition :: [Expr a]
  , statement :: [Statement a]
  } deriving (Show, Eq)

-- | A wrapper to collect all statement types together
data Statement a
  = VarDeclaration (VarDecl a)
  | VarAssignment  (VarAssign a)
  | IfStatement    (IfStatement a) -- FuncCall       (Function a)
  deriving (Show, Eq)

-- | A function definition
data Function a = Function
  { params     :: [VarDecl a]
  , returnType :: Type
  , body       :: [Statement a]
  } deriving (Show, Eq)

-- | The AST node representing the entire program with all of its structures
data Program a = Program
  { statements :: [Statement a]
  , functions  :: [Function a]
  } deriving (Show, Eq)

varDecls :: Program a -> [VarDecl a]
varDecls = map extractDecl . filter isVarDecl . statements
  where
    isVarDecl (VarDeclaration _) = True
    isVarDecl _                  = False

    extractDecl (VarDeclaration x) = x
    extractDecl _                  = undefined

assignments :: Program a -> [VarAssign a]
assignments = map extractAssign . filter isVarAssign . statements
  where
    isVarAssign (VarAssignment _) = True
    isVarAssign _                 = False

    extractAssign (VarAssignment x) = x
    extractAssign _                 = undefined

ifStatements :: Program a -> [IfStatement a]
ifStatements = map extractCond . filter isIfStatement . statements
  where
    isIfStatement (IfStatement _) = True
    isIfStatement _               = False

    extractCond  (IfStatement x) = x
    extractCond  _               = undefined




