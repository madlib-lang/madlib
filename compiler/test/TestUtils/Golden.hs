module TestUtils.Golden (shouldMatchGolden) where

import           Control.Exception              ( evaluate )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                )
import           System.Environment             ( lookupEnv )
import           System.FilePath                ( takeDirectory )
import           Test.Hspec                     ( Expectation
                                                , expectationFailure
                                                , shouldBe
                                                )


-- | Compare actual output against the golden file at the given path.
-- When the UPDATE_GOLDEN environment variable is set, the golden file is
-- (re)written with the actual output and the expectation passes — review the
-- resulting diff with git. A missing golden file fails with instructions
-- instead of being silently created.
shouldMatchGolden :: FilePath -> String -> Expectation
shouldMatchGolden path actual = do
  update <- lookupEnv "UPDATE_GOLDEN"
  case update of
    Just _ -> do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path actual

    Nothing -> do
      exists <- doesFileExist path
      if exists
        then do
          expected <- readFile path
          _        <- evaluate (length expected)
          actual `shouldBe` expected
        else
          expectationFailure
            $  "Missing golden file: "
            <> path
            <> "\nRun the tests with UPDATE_GOLDEN=1 to create it, then review and commit it."
