module CompileSpec where

import qualified Data.Map as M
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
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )

import qualified AST.Source                     as Src
import qualified AST.Solved                     as Slv
import           Infer.Solve
import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Error.Error
import           Explain.Reason
import           AST
import           Compile

snapshotTest :: String -> String -> Golden Text
snapshotTest name actualOutput = Golden
  { output        = pack actualOutput
  , encodePretty  = unpack
  , writeToFile   = T.writeFile
  , readFromFile  = T.readFile
  , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
  , directory     = ".snapshots"
  , failFirstTime = False
  }

-- TODO: Refactor in order to use the inferAST function instead that supports imports
tester :: String -> String
tester code = 
    let inferred = case buildAST "path" code of
            (Right ast) -> runEnv ast >>= (`runInfer` ast)
            _           -> Left $ InferError (UnboundVariable "") NoReason
    in  case inferred of
        Right x -> compile x
        Left e  -> ppShow e
 where
  runEnv x =
    fst <$> runExcept (runStateT (buildInitialEnv x) Unique { count = 0 })

tableTester :: Src.Table -> Src.AST -> Either InferError Slv.Table
tableTester table ast = fst <$> runExcept (runStateT (inferAST "./" table ast) Unique { count = 0 })

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile to JS" $ do
      let code   = unlines 
                    [ "(b, c) => b + c"

                    , "inc :: Num -> Num"
                    , "inc = (x) => x + 1"

                    , "data Maybe a = Just a | Nothing"

                    , "mapMaybe :: (a -> b) -> Maybe a -> Maybe b"
                    , "mapMaybe = (f, m) => where(m) {"
                    , "  is Just x : Just(f(x))"
                    , "  is Nothing: Nothing"
                    , "}"

                    , "might = Just(3)"

                    , "x = where(might) {"
                    , "  is Just x : x"
                    , "  is Nothing: 1"
                    , "}"
                    ]
          actual = tester code
      snapshotTest "should compile to JS" actual
