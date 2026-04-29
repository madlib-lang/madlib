{-# LANGUAGE OverloadedStrings #-}
module Run.PackageSpec where

import           Test.Hspec
import qualified Data.Map.Strict               as Map
import           Data.Version                   ( Version(..) )
import           System.Directory               ( canonicalizePath )
import           System.FilePath                ( joinPath )

import qualified Driver
import qualified Driver.Query                  as Query
import qualified Rock
import           Driver                         ( Prune(..) )
import qualified Utils.PathUtils               as Path
import           Run.Options
import           Run.Target
import           Run.OptimizationLevel
import           Run.SourceMapMode
import           Run.ErrorFormat                ( ErrorFormat(..) )
import           Run.PGOMode                    ( PGOMode(..) )
import           Run.Package                    ( bumpVersion )
import qualified AST.Solved                    as Slv
import           VersionLock.PublicAPI


-- | Type-check a fixture package's main entrypoint via the same Driver path
-- the real `madlib package` command uses, so the resulting Slv.Table contains
-- every package source module that's reachable from main.
typeCheckFixture :: FilePath -> IO (Slv.AST, Slv.Table)
typeCheckFixture casePath = do
  rootPath  <- canonicalizePath casePath
  mainPath  <- canonicalizePath (joinPath [casePath, "src", "Index.mad"])
  let outputPath = joinPath [rootPath, ".tests/build"]
  let options = Options
        { optPathUtils = Path.defaultPathUtils
        , optEntrypoint = mainPath
        , optRootPath = rootPath
        , optOutputPath = outputPath
        , optTarget = TNode
        , optOptimized = False
        , optBundle = False
        , optDebug = False
        , optCoverage = False
        , optGenerateDerivedInstances = True
        , optInsertInstancePlaholders = True
        , optMustHaveMain = False
        , optParseOnly = False
        , optOptimizationLevel = O1
        , optLspMode = False
        , optEmitLLVM = False
        , optSourceMaps = NoSourceMap
        , optErrorFormat = TextFormat
        , optPGOMode = NoPGO
        , optInlineThreshold = Nothing
        }
  state <- Driver.initialState
  (table, _, _) <- Driver.runIncrementalTask state options [] mempty Don'tPrune $ do
    paths <- Rock.fetch $ Query.ModulePathsToBuild mainPath
    solved <- mapM (\p -> do
        (ast, _) <- Rock.fetch $ Query.SolvedASTWithEnv p
        return (p, ast)
      ) paths
    return $ Map.fromList solved
  let Just mainAst = Map.lookup mainPath table
  return (mainAst, table)


fixturePath :: FilePath
fixturePath = "compiler/test/Blackbox/test-cases/package-submodule-imports/madlib_modules/multi-pkg"


spec :: Spec
spec = do
  describe "bumpVersion" $ do
    let v = Version [0, 0, 1] []
    it "bumps major" $
      bumpVersion False Major v `shouldBe` Version [1, 0, 0] []
    it "bumps minor" $
      bumpVersion False Minor v `shouldBe` Version [0, 1, 0] []
    it "bumps patch" $
      bumpVersion False Patch v `shouldBe` Version [0, 0, 2] []
    it "rebuild=True keeps version on Patch" $
      bumpVersion True Patch v `shouldBe` v
    it "rebuild=True keeps version on Minor when patch==0" $
      bumpVersion True Minor (Version [0, 1, 0] []) `shouldBe` Version [0, 1, 0] []
    it "rebuild=True bumps Minor when patch>0" $
      bumpVersion True Minor (Version [0, 1, 3] []) `shouldBe` Version [0, 2, 0] []
    it "rebuild=True keeps version on Major when minor==0 && patch==0" $
      bumpVersion True Major (Version [1, 0, 0] []) `shouldBe` Version [1, 0, 0] []

  describe "computeAPIChange" $ do
    let emptyAPI = PublicAPI mempty mempty mempty mempty mempty
    let api1 = emptyAPI { apiNames = Map.fromList [("foo", "Integer -> Integer")] }
    let api2 = emptyAPI { apiNames = Map.fromList [("foo", "Integer -> Integer"), ("bar", "String -> String")] }
    let api3 = emptyAPI { apiNames = Map.fromList [("foo", "String -> String")] }

    it "returns Patch when APIs are identical" $
      computeAPIChange api1 api1 `shouldBeChange` Patch
    it "returns Minor when only additions" $
      computeAPIChange api1 api2 `shouldBeChange` Minor
    it "returns Major when removed" $
      computeAPIChange api2 api1 `shouldBeChange` Major
    it "returns Major when an existing key changes type" $
      computeAPIChange api1 api3 `shouldBeChange` Major

  describe "buildAPI (multi-module package)" $ do
    it "includes exports from sub-modules namespaced by their relative path" $ do
      (mainAst, table) <- typeCheckFixture fixturePath
      packageRoot      <- canonicalizePath fixturePath
      let api = buildAPI packageRoot mainAst table

      -- Main module exports keep their flat keys.
      Map.lookup "hello" (apiNames api) `shouldSatisfy` (/= Nothing)
      Map.lookup "eval"  (apiNames api) `shouldSatisfy` (/= Nothing)
      Map.lookup "Add"   (apiNames api) `shouldSatisfy` (/= Nothing)

      -- Sub-module exports show up keyed by their package-relative path so
      -- consumers using `from "pkg/Math/Basic"` are tracked too.
      Map.lookup "src/Math/Basic.mad#triple" (apiNames api) `shouldSatisfy` (/= Nothing)
      Map.lookup "src/Math/Basic.mad#double" (apiNames api) `shouldSatisfy` (/= Nothing)
      Map.lookup "src/Math/Basic.mad#eval"   (apiNames api) `shouldSatisfy` (/= Nothing)
      Map.lookup "src/Strings/Util.mad#shout" (apiNames api) `shouldSatisfy` (/= Nothing)
      Map.lookup "src/Strings/Util.mad#greet" (apiNames api) `shouldSatisfy` (/= Nothing)

      -- Re-exported types live in apiTypes under the *defining* sub-module's key,
      -- with the full constructor list. The bare `export type` re-export in the
      -- main module is intentionally not duplicated.
      let opCtors = Map.lookup "src/Math/Basic.mad#Operation" (apiTypes api)
      opCtors `shouldBe` Just ["Add Integer Integer", "Sub Integer Integer", "Mul Integer Integer"]

      let greetingCtors = Map.lookup "src/Strings/Util.mad#Greeting" (apiTypes api)
      greetingCtors `shouldBe` Just ["Hello String", "Hi String"]

  describe "buildAPI sensitivity to sub-module changes (regression)" $ do
    it "computes Minor when a sub-module export is added without touching main" $ do
      (mainAst, table) <- typeCheckFixture fixturePath
      packageRoot      <- canonicalizePath fixturePath
      let prev    = buildAPI packageRoot mainAst table
      let added   = prev { apiNames = Map.insert "src/Math/Basic.mad#cube" "Integer -> Integer" (apiNames prev) }
      computeAPIChange prev added `shouldBeChange` Minor

    it "computes Major when a sub-module export is removed" $ do
      (mainAst, table) <- typeCheckFixture fixturePath
      packageRoot      <- canonicalizePath fixturePath
      let prev    = buildAPI packageRoot mainAst table
      let removed = prev { apiNames = Map.delete "src/Math/Basic.mad#triple" (apiNames prev) }
      computeAPIChange prev removed `shouldBeChange` Major

    it "computes Patch when only main-module exports vary in unrelated metadata" $ do
      (mainAst, table) <- typeCheckFixture fixturePath
      packageRoot      <- canonicalizePath fixturePath
      let prev = buildAPI packageRoot mainAst table
      computeAPIChange prev prev `shouldBeChange` Patch


-- | Unit-test helper: APIChange has no Eq/Show instance, so we map it to a
-- string in the assertion to get a useful failure message from hspec.
shouldBeChange :: APIChange -> APIChange -> Expectation
shouldBeChange actual expected = describe' actual `shouldBe` describe' expected
  where
    describe' :: APIChange -> String
    describe' Major = "Major"
    describe' Minor = "Minor"
    describe' Patch = "Patch"
