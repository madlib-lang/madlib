module Infer.Env where

import qualified Data.Map as M
import           Infer.Type
import           Grammar

type Vars = M.Map String Scheme
type ADTs = M.Map String Type
type Typings = M.Map String Scheme
type Imports = M.Map Name Type


data Env
  = Env
    { envvars :: Vars
    , envadts :: ADTs
    , envtypings :: Typings
    , envimports :: Imports
    }
    deriving(Eq, Show)

extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envvars = M.insert x s $ envvars env }


extendTypings :: Env -> (String, Scheme) -> Env
extendTypings env (x, s) = env { envtypings = M.insert x s $ envtypings env }
