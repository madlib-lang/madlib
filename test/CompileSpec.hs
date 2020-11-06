module CompileSpec where

import qualified Data.Map                      as M
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

import qualified AST.Source                    as Src
import qualified AST.Solved                    as Slv
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
        Left  e -> ppShow e
 where
  runEnv x =
    fst <$> runExcept (runStateT (buildInitialEnv x) Unique { count = 0 })

tableTester :: Src.Table -> Src.AST -> String
tableTester table ast =
  let
    resolved = fst
      <$> runExcept (runStateT (inferAST "./" table ast) Unique { count = 0 })
  in  case resolved of
        Right x -> concat $ compile <$> M.elems x
        Left  e -> ppShow e

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile to JS" $ do
      let code = unlines
            [ "export fn = (b, c) => b + c"
            , "inc :: Num -> Num"
            , "inc = (x) => x + 1"
            , "dec :: Num -> Num"
            , "dec = (x) => x - 1"
            , "double :: Num -> Num"
            , "double = (x) => x * 2"
            , "half :: Num -> Num"
            , "half = (x) => x / 2"
            , "true  = True"
            , "false = False"
            , "3 |> half |> double"
            , "3 == 5"
            , "response     = { users: [] }"
            , "users        = response.users"
            , "carResponse  = { cars: [] }"
            , "allResponses = { ...response, ...carResponse }"
            , "where(allResponses) {"
            , "  is { users: List a, cars: cs }: cs"
            , "  is _                          : []"
            , "}"
            , "where(3) {"
            , "  is Num: 3"
            , "}"
            , "where(\"3\") {"
            , "  is String: 3"
            , "}"
            , "where(True) {"
            , "  is Bool: 3"
            , "}"
            , "where(3) {"
            , "  is 3: 3"
            , "}"
            , "where(\"3\") {"
            , "  is \"3\": 3"
            , "}"
            , "where(True) {"
            , "  is True: 3"
            , "}"
            , "log :: a -> a"
            , "log = (a) => #- { console.log(a); return a; } -#"
            , "if (True) { \"OK\" } else { \"NOT OK\" }"
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

    it "should compile imports and exports" $ do
      let codeA = "export singleton = (a) => [a]"
          astA  = buildAST "./ModuleA.mad" codeA
          codeB = unlines
            [ "import L from \"ModuleA\""
            , "import { singleton } from \"ModuleA\""
            , "L.singleton(3)"
            ]
          astB   = buildAST "./ModuleB.mad" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable =
                      M.fromList [("./ModuleA.mad", a), ("./ModuleB.mad", b)]
              in  tableTester astTable b
      snapshotTest "should compile imports and exports" actual
