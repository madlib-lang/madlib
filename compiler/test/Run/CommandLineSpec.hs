{-# LANGUAGE LambdaCase #-}
module Run.CommandLineSpec where

import           Options.Applicative
import           Test.Hspec

import           Run.CommandLine


parseCommandArgs :: [String] -> ParserResult Command
parseCommandArgs = execParserPure defaultPrefs (info parseTransform fullDesc)


spec :: Spec
spec = describe "madlib install conflict options" $ do
  it "is non-interactive by default" $
    parseCommandArgs ["install"] `shouldSatisfy` \case
      Success Install { installInteractive = False, installResolutions = [] } -> True
      _ -> False

  it "parses --interactive" $
    parseCommandArgs ["install", "--interactive"] `shouldSatisfy` \case
      Success Install { installInteractive = True } -> True
      _ -> False

  it "preserves repeated --resolve values" $
    parseCommandArgs
      ["install", "--resolve", "study@0.0.6", "--resolve", "http@1.2.3"]
      `shouldSatisfy` \case
        Success Install { installResolutions = values } ->
          values == ["study@0.0.6", "http@1.2.3"]
        _ -> False
