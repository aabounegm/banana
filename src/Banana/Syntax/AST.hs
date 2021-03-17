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

-- | Function call, with the same representation as for expressions
data FuncCall a = FuncCall
  { func :: a        -- A reference to the function declaration
  , args :: [Expr a] -- The arguments provided to the invocation
  } deriving (Show, Eq)

-- | An arithmetic expression
data Expr a = Add   (Expr a) (Expr a)   -- ^ Addition (+)
            | Sub   (Expr a) (Expr a)   -- ^ Subtraction (-)
            | Mul   (Expr a) (Expr a)   -- ^ Multiplication (*)
            | Div   (Expr a) (Expr a)   -- ^ Division (/)
            | Var   a                   -- ^ Variable (identifier)
            | Lit   Double              -- ^ A literal value
            | Not   (Expr a)            -- ^ Unary logical not ('not')
            | Or    (Expr a) (Expr a)   -- ^ Logical OR ('or')
            | And   (Expr a) (Expr a)   -- ^ Logical AND ('and')
            | Eq    (Expr a) (Expr a)   -- ^ Equal to (=)
            | NEq (Expr a) (Expr a)     -- ^ Not Equal to (/=)
            | Less  (Expr a) (Expr a)   -- ^ Less than (<)
            | More  (Expr a) (Expr a)   -- ^ Bigger than (>)
            | LEq   (Expr a) (Expr a)   -- ^ Less than or Equal to (<=)
            | MEq   (Expr a) (Expr a)   -- ^ Bigger than or Equal to (>=)
            | FuncCallExpr (FuncCall a) -- ^ A function call as an expression
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
  | FuncCallStatement (FuncCall a)
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


