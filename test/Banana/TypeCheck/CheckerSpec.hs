module Banana.TypeCheck.CheckerSpec (spec) where

import           Banana.Syntax.AST
import           Banana.TypeCheck.Checker
import           Test.Hspec

spec :: Spec
spec = do
  describe "TypeCheck" $ do
    it "Passes programs with all variables declared" $ do
      let program = Program
            { functions = []
            , statements =
              [ VarDeclaration $ VarDecl "x" Number
              , VarAssignment  $ VarAssign (Var "x") (Lit 5.0)
              , FuncCallStatement $ FuncCall "print" [Add (Var "x") (Lit 42.0)]
              ]
            }
      allVarsDeclared program `shouldBe` True

    it "Fails on undeclared variables in assignment" $ do
      let program = Program
            { functions = []
            , statements =
              [ VarDeclaration $ VarDecl "x" Number
              , VarAssignment  $ VarAssign (Var "x") (Var "y")
              ]
            }
      allVarsDeclared program `shouldBe` False

    it "Fails on undeclared variables in function call" $ do
      let program = Program
            { functions = []
            , statements =
              [ VarDeclaration    $ VarDecl "x" Number
              , FuncCallStatement $ FuncCall "print" [Add (Var "x") (Var "y")]
              ]
            }
      allVarsDeclared program `shouldBe` False

    it "Accepts programs with unique declarations" $ do
      let program = Program
            { functions = []
            , statements =
              [ VarDeclaration $ VarDecl "x" Number
              , VarDeclaration $ VarDecl "y" Number
              , VarAssignment  $ VarAssign (Var "x") (Var "y")
              ]
            }
      varDeclsAreUnique program `shouldBe` True

    it "Fails on repeated declarations" $ do
      let program = Program
            { functions = []
            , statements =
              [ VarDeclaration $ VarDecl "x" Number
              , VarAssignment  $ VarAssign (Var "x") (Lit 1.0)
              , VarDeclaration $ VarDecl "x" Number
              ]
            }
      varDeclsAreUnique program `shouldBe` False

    it "Fails when any check fails" $ do
      let program = Program
            { functions = []
            , statements =
              [ FuncCallStatement (FuncCall "print" [Var "x"])
              ]
            }
      checkAll program `shouldBe` False

