{-# LANGUAGE NamedFieldPuns #-}
module Infer.MonomorphizationState where

import           Infer.Type
import qualified Data.Map as Map
import Data.IORef
import GHC.IO (unsafePerformIO)
import AST.Solved


monomorphizationState :: IORef (Map.Map FunctionId MonomorphizationRequest)
{-# NOINLINE monomorphizationState #-}
monomorphizationState = unsafePerformIO $ newIORef Map.empty

data MonomorphizationRequest
  = MonomorphizationRequest
  { mrIndex :: Int
  , mrResult :: Maybe Exp
  }
  deriving(Eq, Ord)

data FunctionId
  = FunctionId
  { fiFunctionName :: String
  , fiModulePath :: FilePath
  , fiMonomorphicType :: Type
  }
  deriving(Eq, Ord)

newtype MonomorphizationState
  = MonomorphizationState
  { msRequests :: Map.Map FunctionId MonomorphizationRequest
  }
  deriving(Eq, Ord)

makeMonomorphizedName :: String -> FilePath -> Type -> IO String
makeMonomorphizedName fnName modulePath t = do
  state <- readIORef monomorphizationState
  let fnId = FunctionId fnName modulePath t
  case Map.lookup fnId state of
    Just MonomorphizationRequest { mrIndex } ->
      return $ fnName ++ "__" ++ show mrIndex

    Nothing ->
      return fnName

newRequest :: String -> FilePath -> Type -> IO String
newRequest fnName modulePath t = do
  state <- readIORef monomorphizationState
  let nextIndex = Map.size state
  let fnId = FunctionId fnName modulePath t
  let req = MonomorphizationRequest nextIndex Nothing
  let newState = Map.insert fnId req state
  writeIORef monomorphizationState newState
  makeMonomorphizedName fnName modulePath t

setRequestResult :: String -> FilePath -> Type -> Exp -> IO ()
setRequestResult fnName modulePath t result = do
  state <- readIORef monomorphizationState
  let fnId = FunctionId fnName modulePath t
  let updatedReq = case Map.lookup fnId state of
        Just req ->
          req{ mrResult = Just result }

        Nothing ->
          undefined
  let updatedState = Map.insert fnId updatedReq state
  writeIORef monomorphizationState updatedState
