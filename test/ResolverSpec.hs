module ResolverSpec where

import qualified Data.Map                      as M
import qualified Data.Either                   as E
import           Grammar
import           Lexer
import           Resolver
import           AST
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import           Test.Hspec.Golden              ( Golden(..) )
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Validate         ( runValidateT )
import           Control.Monad.Reader           ( runReader
                                                , MonadReader(..)
                                                )

snapshotTest :: Show a => String -> a -> Golden Text
snapshotTest name actualOutput = Golden
  { output        = pack $ ppShow actualOutput
  , encodePretty  = ppShow
  , writeToFile   = T.writeFile
  , readFromFile  = T.readFile
  , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
  , directory     = ".snapshots"
  , failFirstTime = False
  }

tester :: String -> Either [RError] AST
tester code =
  let resolveM = case buildAST "path" code of
        (Right ast) -> doResolve ast
        _           -> rRefute $ TypeError "" ""
  in  runReader (runValidateT resolveM) initialEnv
  where initialEnv = Env M.empty M.empty Nothing (Backtrace [])

astA :: AST
(Right astA) = buildAST "fixtures/sourceA.mad" $ unlines
  [ "import \"sourceB\""
  , "fn :: Num -> Num -> Num"
  , "fn = (a, b) => fn2(a, b) + a"
  ]

astB :: AST
(Right astB) = buildAST "fixtures/sourceB.mad"
  $ unlines ["fn2 :: Num -> Num -> Num", "fn2 = (a, b) => a + b"]

astC :: AST
(Right astC) = buildAST "src/sourceC.mad" $ unlines
  [ "import \"sourceD\""
  , "fn :: Num -> Num -> Num"
  , "fn = (a, b) => fn2(a, b) + a"
  ]

astD :: AST
(Right astD) = buildAST "src/sourceD.mad"
  $ unlines ["fn2 :: Num -> Num -> Num", "fn2 = (a, b) => a + b"]


spec :: Spec
spec = do
  describe "resolve" $ do
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

    it
        "should return a FunctionNotFound error if a called function isn't in the environment"
      $ do
          let
            code = unlines
              ["fn2 :: Num -> Num -> Num", "fn2 = (a, b) => fn(a, b) + a"]
            Left actual = tester code
          snapshotTest
            "should return a FunctionNotFound error if a called function isn't in the environment"
            actual

    it "should stack errors correctly" $ do
      let code = unlines
            [ "fn2 :: Num -> Num -> Num -> Num"
            , "fn2 = (a, b) => fn(a + \"Wrong\", b) + a"
            ]
          Left actual = tester code
      snapshotTest "should stack errors correctly" actual

    it "should resolve Bool === Bool" $ do
      let code =
            unlines ["eq :: Bool -> Bool -> Bool", "eq = (a, b) => a === b"]
          actual = case tester code of
            (Right _) -> True
            _         -> False
      actual `shouldBe` True

    it "should give a type error for Bool === Num" $ do
      let code =
            unlines ["eq :: Bool -> Num -> Bool", "eq = (a, b) => a === b"]
          Left actual = tester code
      snapshotTest "should give a type error for Bool === Num" actual

    it "should give a type error for Num === Bool" $ do
      let code =
            unlines ["eq :: Num -> Bool -> Bool", "eq = (a, b) => a === b"]
          Left actual = tester code
      snapshotTest "should give a type error for Num === Bool" actual

  describe "resolveASTTable" $ do
    it "should resolve an AST table" $ do
      let astTable = M.fromList
            [("fixtures/sourceA.mad", astA), ("fixtures/sourceB.mad", astB)]
          env = Env { vtable    = M.empty
                    , ftable    = M.empty
                    , backtrace = Backtrace []
                    , rootPath  = Nothing
                    }
          actual = resolveASTTable env astA astTable
      actual `shouldBe` expected1

    it "should fail if the AST does not have a path value" $ do
      let corrupted = astA { apath = Nothing }
          astTable =
            M.fromList
              [ ("fixtures/sourceA.mad", corrupted)
              , ("fixtures/sourceB.mad", astB)
              ]
          env = Env { vtable    = M.empty
                    , ftable    = M.empty
                    , backtrace = Backtrace []
                    , rootPath  = Nothing
                    }
          Left actual = resolveASTTable env corrupted astTable
      snapshotTest "should fail if the AST does not have a path value" actual

    it "should resolve an AST table and figure out the root path" $ do
      let astTable =
            M.fromList [("src/sourceC.mad", astC), ("src/sourceD.mad", astD)]
          env = Env { vtable    = M.empty
                    , ftable    = M.empty
                    , backtrace = Backtrace []
                    , rootPath  = Nothing
                    }
          actual = resolveASTTable env astC astTable
      actual `shouldBe` expected2

    it
        "should give an error if parameter count does not match the one of the signature"
      $ do
          let
            code =
              unlines ["fn :: Num -> Num -> Num -> Num", "fn = (a, b) => a + b"]
            Left actual = tester code
          snapshotTest
            "should give an error if parameter count does not match the one of the signature"
            actual

    it "should aggregate errors if more than one error occurs" $ do
      let code = unlines
            [ "fn :: Num -> Num -> Num -> Num"
            , "fn = (a, b) => a + b"
            , "fn2 :: Num -> Num -> Num -> Num -> Num"
            , "fn2 = (a, b) => a + b"
            , "fn3 :: Num -> Num -> Num"
            , "fn3 = (a, b) => a + b"
            , "fn4 :: Num -> Num -> Num"
            , "fn4 = (a, b) => a + b"
            ]
          Left actual = tester code
      snapshotTest "should aggregate errors if more than one error occurs"
                   actual


expected1 = Right $ M.fromList
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

expected2 = Right $ M.fromList
  [ ( "src/sourceC.mad"
    , AST
      { aimports   = [ImportDecl { ipos = Pos 0 1 1, ipath = "sourceD" }]
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
      , apath      = Just "src/sourceC.mad"
      }
    )
  , ( "src/sourceD.mad"
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
      , apath      = Just "src/sourceD.mad"
      }
    )
  ]
