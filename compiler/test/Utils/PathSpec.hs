{-# LANGUAGE FlexibleInstances #-}
module Utils.PathSpec where

import           Utils.Path
import qualified Utils.PathUtils               as PathUtils
import           Parse.Madlib.AST               ( computeAbsoluteImportPaths )
import           AST.Source                     ( Source(..)
                                                , SourceTarget(TargetAll)
                                                , Import_(DefaultImport)
                                                )
import           Error.Context                  ( Context(Context) )
import           Error.Error                    ( CompilationError(CompilationError)
                                                , TypeError(ImportNotFound)
                                                )
import           Explain.Location               ( emptyArea )
import           Test.Hspec
import           System.Directory               ( canonicalizePath )
import           System.FilePath                ( joinPath )

spec :: Spec
spec = do
  describe "computeRootPath" $ do
    it "should build return the path part before the filename" $ do
      computeRootPath "some/folder/file.mad" `shouldBe` "some/folder/"

  describe "resolveAbsoluteSrcPath" $ do
    let fixturePath = "compiler/test/Blackbox/test-cases/import-alias-resolution"

    it "resolves an existing aliased import" $ do
      rootPath <- canonicalizePath fixturePath
      expected <- canonicalizePath $ joinPath [fixturePath, "src", "Existing.mad"]

      resolved <- resolveAbsoluteSrcPath PathUtils.defaultPathUtils rootPath "@/Existing"

      resolved `shouldBe` Just expected

    it "does not resolve a missing aliased import" $ do
      rootPath <- canonicalizePath fixturePath

      resolved <- resolveAbsoluteSrcPath PathUtils.defaultPathUtils rootPath "@/Missing"

      resolved `shouldBe` Nothing

    it "reports a missing aliased import as ImportNotFound" $ do
      rootPath <- canonicalizePath fixturePath
      let entrypoint = joinPath [rootPath, "Entrypoint.mad"]
          namespace  = Source emptyArea TargetAll "Missing"
          missingImport = Source emptyArea TargetAll $ DefaultImport namespace "@/Missing" "@/Missing"

      result <- computeAbsoluteImportPaths PathUtils.defaultPathUtils True entrypoint rootPath [missingImport]

      result `shouldBe`
        Left (CompilationError (ImportNotFound "@/Missing") (Context entrypoint emptyArea))
