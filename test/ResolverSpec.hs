module ResolverSpec where

import qualified Data.Map                      as M
import qualified Data.Either                   as E
import           Grammar
import           Lexer
import           Resolver
import           Test.Hspec
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except
import           Control.Monad.Trans.Except

tester :: String -> Either TCError AST
tester code = runExcept $ case buildAST "path" code of
  (Right ast) -> resolve initialEnv ast
  _           -> throwError $ TypeError "" ""
  where initialEnv = Env M.empty M.empty

(Right astA) = buildAST "fixtures/sourceA.mad" $ unlines
  [ "import \"sourceB\""
  , "fn :: Num -> Num -> Num"
  , "fn = (a, b) => fn2(a, b) + a"
  ]

(Right astB) = buildAST "fixtures/sourceB.mad"
  $ unlines ["fn2 :: Num -> Num -> Num", "fn2 = (a, b) => a + b"]


spec :: Spec
spec = do
  describe "Resolver" $ do
    it "should resolve functions that add parameters" $ do
      let code   = unlines ["fn :: Num -> Num -> Num", "fn = (a, b) => a + b"]
          actual = case tester code of
            (Right _) -> True
            _         -> False
      actual `shouldBe` True


    it "should resolve function calls" $ do
      let code = unlines
            [ "fn :: Num -> Num -> Num"
            , "fn = (a, b) => a + b"
            , "fn2 :: Num -> Num -> Num"
            , "fn2 = (a, b) => fn(a, b) + a"
            ]
          actual = case tester code of
            (Right _) -> True
            _         -> False
      actual `shouldBe` True


    it "should resolve an AST table" $ do
      let astTable = M.fromList
            [("fixtures/sourceA.mad", astA), ("fixtures/sourceB.mad", astB)]
          env    = Env { vtable = M.empty, ftable = M.empty }
          result = resolveASTTable env astA astTable
      result `shouldBe` expected


    it
        "should give an error if parameter count does not match the one of the signature"
      $ do
          let
            code =
              unlines ["fn :: Num -> Num -> Num -> Num", "fn = (a, b) => a + b"]
            actual = tester code
          actual `shouldBe` Left (SignatureError 3 2)


expected = M.fromList
  [ ( "fixtures/sourceA.mad"
    , AST
      { aimports   = [ImportDecl { ipos = Pos 0 1 1, ipath = "sourceB" }]
      , afunctions =
        [ FunctionDef
            { ftype    = Just "Num"
            , ftypeDef = Just Typing { tpos   = Pos 17 2 1
                                     , tfor   = "fn"
                                     , ttypes = ["Num", "Num", "Num"]
                                     }
            , fpos     = Pos 17 2 1
            , fname    = "fn"
            , fparams  = ["a", "b"]
            , fbody    = Body Operation
              { etype     = Just "Num"
              , epos      = Pos 56 3 16
              , eleft     = FunctionCall
                { etype = Just "Num"
                , epos  = Pos 56 3 16
                , ename = "fn2"
                , eargs = [ VarAccess { etype = Just "Num"
                                      , epos  = Pos 60 3 20
                                      , ename = "a"
                                      }
                          , VarAccess { etype = Just "Num"
                                      , epos  = Pos 63 3 23
                                      , ename = "b"
                                      }
                          ]
                }
              , eoperator = Plus
              , eright    = VarAccess { etype = Just "Num"
                                      , epos  = Pos 68 3 28
                                      , ename = "a"
                                      }
              }
            }
        ]
      , apath      = Just "fixtures/sourceA.mad"
      }
    )
  , ( "fixtures/sourceB.mad"
    , AST
      { aimports   = []
      , afunctions =
        [ FunctionDef
            { ftype    = Just "Num"
            , ftypeDef = Just Typing { tpos   = Pos 0 1 1
                                     , tfor   = "fn2"
                                     , ttypes = ["Num", "Num", "Num"]
                                     }
            , fpos     = Pos 0 1 1
            , fname    = "fn2"
            , fparams  = ["a", "b"]
            , fbody    = Body Operation
              { etype     = Just "Num"
              , epos      = Pos 41 2 17
              , eleft     = VarAccess { etype = Just "Num"
                                      , epos  = Pos 41 2 17
                                      , ename = "a"
                                      }
              , eoperator = Plus
              , eright    = VarAccess { etype = Just "Num"
                                      , epos  = Pos 45 2 21
                                      , ename = "b"
                                      }
              }
            }
        ]
      , apath      = Just "fixtures/sourceB.mad"
      }
    )
  ]
