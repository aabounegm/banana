module Banana.Syntax.ParserSpec (spec) where

import           Banana.Syntax.AST
import           Banana.Syntax.Parser
import           Test.Hspec
import           Text.Trifecta        (parseString)
import           Text.Trifecta.Parser (Parser)
import           Text.Trifecta.Result (Result (Failure, Success))


shouldParseAs :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
shouldParseAs parser input expectation = parsed `shouldBe` Right expectation
  where
    parsed = case parseString parser mempty input of
      Success x -> Right x
      Failure e -> Left (show e)

shouldParseAsExpr :: String -> Expr String -> Expectation
shouldParseAsExpr = shouldParseAs parseExpr

shouldParseAsType :: String -> Type -> Expectation
shouldParseAsType = shouldParseAs parseType

shouldParseAsVarDecl :: String -> VarDecl String -> Expectation
shouldParseAsVarDecl = shouldParseAs parseVarDecl

shouldParseAsVarAssign :: String -> VarAssign String -> Expectation
shouldParseAsVarAssign = shouldParseAs parseVarAssign

shouldParseAsProgram :: String -> Program String -> Expectation
shouldParseAsProgram = shouldParseAs parseProgram

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses \"1+1\" successfully" $ do
      "1+1\n" `shouldParseAsExpr` Add (Lit 1.0) (Lit 1.0)

    it "parses \"x+(5*2)-(x+2)\" successfully" $ do
      "x+(5*2)-(x+2)\n" `shouldParseAsExpr` Sub
        (Add (Var "x") (Mul (Lit 5.0) (Lit 2.0)))
        (Add (Var "x") (Lit 2.0))

    it "parses function calls in expressions" $ do
      "x + func(5) * f2(42, x)" `shouldParseAsExpr` Add
        (Var "x")
        (Mul
          (FuncCallExpr (FuncCall "func" [Lit 5.0]))
          (FuncCallExpr (FuncCall "f2" [Lit 42.0, Var "x"]))
        )

    it "parses \"x and y\" successfully" $ do
      "x and y\n" `shouldParseAsExpr` And (Var "x") (Var "y")

    it "parses \"x or y\" successfully" $ do
      "x or y\n" `shouldParseAsExpr` Or (Var "x") (Var "y")

    it "parses \"not y\" successfully" $ do
      "not y\n" `shouldParseAsExpr` Not (Var "y")

    it "parses \"4<5\" successfully" $ do
      "4<5\n" `shouldParseAsExpr` Less (Lit 4.0) (Lit 5.0)

    it "parses \"3>1\" successfully" $ do
      "3>1\n" `shouldParseAsExpr` More (Lit 3.0) (Lit 1.0)

    it "parses \"x<=1\" successfully" $ do
      "x<=1\n" `shouldParseAsExpr` LEq (Var "x") (Lit 1.0)

    it "parses \"x>=1\" successfully" $ do
      "x>=1\n" `shouldParseAsExpr` MEq (Var "x") (Lit 1.0)

    it "parses \"1=1\" successfully" $ do
      "1=1\n" `shouldParseAsExpr` Eq (Lit 1.0) (Lit 1.0)

    it "parses \"2/=1\" successfully" $ do
      "2/=1\n" `shouldParseAsExpr` NEq (Lit 2.0) (Lit 1.0)

    it "preserves order of operations" $ do
      "10 - x * 5 = 0 and y <= 3 or 0 /= z" `shouldParseAsExpr` Or
        (And
          (Eq (Sub (Lit 10.0) (Mul (Var "x") (Lit 5.0))) (Lit 0.0))
          (LEq (Var "y") (Lit 3.0))
        )
        (NEq (Lit 0.0) (Var "z"))

  describe "parseType" $ do
    it "parses \"num\" successfully" $ do
      "num" `shouldParseAsType` Number

    it "parses \"array 5 num\" successfully" $ do
      "array 5 num" `shouldParseAsType` Array 5 Number

    it "parses \"array 5 array 2 array 3 num\" successfully" $ do
      "array 5 array 2 array 3 num" `shouldParseAsType` Array 5 (Array 2 (Array 3 Number))

  describe "parseVarDecl" $ do
    it "parses \"var x: num\" successfully" $ do
      "var x: num" `shouldParseAsVarDecl` VarDecl "x" Number

    it "parses \"var _CAPS123_ : array 123 num\" successfully" $ do
      "var _CAPS123_ : array 123 num" `shouldParseAsVarDecl` VarDecl "_CAPS123_" (Array 123 Number)

    it "parses \"var ans2:array 5 array 1 num\" successfully" $ do
      "var ans2:array 5 array 1 num" `shouldParseAsVarDecl` VarDecl "ans2" (Array 5 (Array 1 Number))

  describe "parseVarAssign" $ do
    it "parses \"x := 123\" successfully" $ do
      "x := 123" `shouldParseAsVarAssign` VarAssign (Var "x") (Lit 123.0)

    it "parses \"myVar5 := 2+5*myVar5\" successfully" $ do
      "myVar5 := 2+5*myVar5" `shouldParseAsVarAssign` VarAssign
        (Var "myVar5")
        (Add (Lit 2.0) (Mul (Lit 5.0) (Var "myVar5")))

  describe "parseProgram" $ do
    it "parses a series of variable declarations successfully" $ do
      "var x: num\nvar y: array 5 num\n" `shouldParseAsProgram` Program
        { functions = []
        , statements =
            [ VarDeclaration $ VarDecl "x" Number
            , VarDeclaration $ VarDecl "y" (Array 5 Number)
            ]
        }

    it "parses a series of assignments successfully" $ do
      "x := 42\npi:=22/7\n" `shouldParseAsProgram` Program
        { functions = []
        , statements =
            [ VarAssignment $ VarAssign (Var "x") (Lit 42.0)
            , VarAssignment $ VarAssign (Var "pi") (Div (Lit 22.0) (Lit 7.0))
            ]
        }

    it "parses lines of interleaved assignments and declarations" $ do
      "var x: num\nx:=4+y\nvar y: array 1 num\ny := 5" `shouldParseAsProgram` Program
        { functions = []
        , statements =
            [ VarDeclaration $ VarDecl "x" Number
            , VarAssignment  $ VarAssign (Var "x") (Add (Lit 4.0) (Var "y"))
            , VarDeclaration $ VarDecl "y" (Array 1 Number)
            , VarAssignment  $ VarAssign (Var "y") (Lit 5.0)
            ]
        }

    it "parses function calls as statements" $ do
      "print(42)\nmyFunc23(-1)" `shouldParseAsProgram` Program
        { functions = []
        , statements =
            [ FuncCallStatement (FuncCall "print" [Lit 42])
            , FuncCallStatement (FuncCall "myFunc23" [Lit (-1)])
            ]
        }
