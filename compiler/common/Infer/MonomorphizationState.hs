{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Infer.MonomorphizationState where

import           Infer.Type
import qualified Data.Map as Map
import Data.IORef
import GHC.IO (unsafePerformIO)
import AST.Solved
import qualified Data.Set as Set
import           Data.Hashable
import           GHC.Generics hiding(Constructor)

data ImportType
  = DefinitionImport Int
  | ConstructorImport
  | ExpressionImport
  deriving(Eq, Ord, Show, Generic, Hashable)

monomorphizationState :: IORef (Map.Map FunctionId MonomorphizationRequest)
{-# NOINLINE monomorphizationState #-}
monomorphizationState = unsafePerformIO $ newIORef Map.empty

-- Outer Map is where the import is needed
-- Inner Map is where the imported names come from and gives a list of names to be imported
monomorphizationImports :: IORef (Map.Map FilePath (Map.Map FilePath (Set.Set (String, Type, ImportType))))
{-# NOINLINE monomorphizationImports #-}
monomorphizationImports = unsafePerformIO $ newIORef Map.empty

-- TODO: this can most likely go away!
monomorphicMethods :: IORef (Set.Set String)
{-# NOINLINE monomorphicMethods #-}
monomorphicMethods = unsafePerformIO $ newIORef Set.empty

data ScopeState
  = ScopeState
      { ssRequests :: Map.Map FunctionId MonomorphizationRequest
      , ssDefinitions :: Map.Map String Exp
      }
      deriving(Eq, Ord, Show)

-- State for local functions that need to be monomorphized
-- Each item in the list is a full state for a given scope
-- the last one is the current scope and the first one is
-- the one in the highest level in the function body.
makeLocalMonomorphizationState :: () -> IORef [ScopeState]
{-# NOINLINE makeLocalMonomorphizationState #-}
makeLocalMonomorphizationState _ = unsafePerformIO $ newIORef []

data MonomorphizationRequest
  = MonomorphizationRequest
  { mrIndex :: Int
  , mrResult :: Maybe Exp
  , mrIsNullaryMethod :: Bool
  }
  deriving(Eq, Ord, Show, Generic, Hashable)

data FunctionId
  = FunctionId
  { fiFunctionName :: String
  , fiModulePath :: FilePath
  , fiMonomorphicType :: Type
  }
  deriving(Eq, Ord, Show, Generic, Hashable)


buildMonomorphizedName :: String -> Int -> String
buildMonomorphizedName fnName (-1) = fnName
buildMonomorphizedName fnName index =
  fnName ++ "__" ++ show index


makeMonomorphizedName :: String -> FilePath -> Type -> IO String
makeMonomorphizedName fnName modulePath t = do
  state <- readIORef monomorphizationState
  let fnId = FunctionId fnName modulePath t
  case Map.lookup fnId state of
    Just MonomorphizationRequest { mrIndex } ->
      return $ buildMonomorphizedName fnName mrIndex

    Nothing ->
      return fnName

newRequest :: String -> FilePath -> Bool -> Type -> IO String
newRequest fnName modulePath isNullaryMethod t = do
  atomicModifyIORef
    monomorphizationState
    (\state ->
      let nextIndex = Map.size state
          fnId = FunctionId fnName modulePath t
          req = MonomorphizationRequest nextIndex Nothing isNullaryMethod
      in  (Map.insert fnId req state, ())
    )

  makeMonomorphizedName fnName modulePath t

setRequestResult :: String -> FilePath -> Bool -> Type -> Exp -> IO ()
setRequestResult fnName modulePath isNullaryMethod t result = do
  atomicModifyIORef
    monomorphizationState
    (\state ->
      let fnId = FunctionId fnName modulePath t
          updatedReq = case Map.lookup fnId state of
            Just req ->
              req{ mrResult = Just result, mrIsNullaryMethod = isNullaryMethod }

            Nothing ->
              MonomorphizationRequest (-1) (Just result) isNullaryMethod
          updatedState = Map.insert fnId updatedReq state
      in  (updatedState, ())
    )
