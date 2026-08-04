module Run.LanguageServer.DiagnosticsSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldSatisfy
                                                , shouldBe
                                                )
import qualified Data.Text                      as T
import           Language.LSP.Types
import           Error.Error
import           Error.Context
import           Explain.Location
import           Infer.Type
import           Run.LanguageServer.Diagnostics  ( errorToDiagnostic )


makeCtx :: Context
makeCtx = Context "test/Module.mad" (Area (Loc 0 1 1) (Loc 0 1 20))

secondaryLoc :: SecondaryLocation
secondaryLoc = SecondaryLocation "test/Module.mad" (Area (Loc 0 5 1) (Loc 0 5 16)) "'f' is applied here"


relatedInfoOf :: Diagnostic -> Maybe (List DiagnosticRelatedInformation)
relatedInfoOf (Diagnostic _ _ _ _ _ _ related) = related


spec :: Spec
spec = describe "errorToDiagnostic" $ do
  it "has no relatedInformation when the error carries no secondary marker" $ do
    let err = CompilationError (UnificationError (TypeMismatch tStr tFloat NoOrigin [])) makeCtx
    diag <- errorToDiagnostic err
    relatedInfoOf diag `shouldBe` Nothing

  it "populates relatedInformation from a secondary marker" $ do
    let err = CompilationError
          (UnificationError (TypeMismatch tStr tFloat (FromFunctionArgument "f" 1 Nothing) [secondaryLoc]))
          makeCtx
    diag <- errorToDiagnostic err
    case relatedInfoOf diag of
      Just (List xs) -> do
        length xs `shouldSatisfy` (> 0)
        let messages = map (\(DiagnosticRelatedInformation _ msg) -> msg) xs
        messages `shouldSatisfy` any (\m -> T.pack "applied here" `T.isInfixOf` m)
      _ -> error "expected populated relatedInformation"
