{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.Env where

import           Canonicalize.CanonicalM
import           Infer.Type
import           Error.Error
import           Explain.Reason
import           Control.Monad.Except
import qualified Data.Map                      as M


data Interface = Interface [TVar] [Pred] deriving(Eq, Show)

type TypeDecls = M.Map String Type
type Interfaces = M.Map String Interface

data Env
  = Env
    { envTypeDecls   :: TypeDecls
    , envInterfaces  :: Interfaces
    , envCurrentPath :: FilePath
    }
    deriving(Eq, Show)



addADT :: Env -> String -> Type -> Env
addADT env name adt =
  let adts         = envTypeDecls env
      withAddition = M.insert name adt adts
  in  env { envTypeDecls = withAddition }


lookupADT :: Env -> String -> CanonicalM Type
lookupADT env name = case M.lookup name (envTypeDecls env) of
  Just found -> return found
  Nothing    -> throwError $ InferError (UnknownType name) NoReason


initialEnv :: Env
initialEnv = Env
  { envTypeDecls   = M.fromList
                       [ ("List" , tList)
                       , ("(,)"  , tTuple2)
                       , ("(,,)" , tTuple3)
                       , ("(,,,)", tTuple4)
                       ]
  , envInterfaces  = mempty
  , envCurrentPath = ""
  }

initialWithPath :: FilePath -> Env
initialWithPath astPath = initialEnv { envCurrentPath = astPath }
