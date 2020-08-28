module Wrapper
  ( parse
  , Error(..)
  , ErrClass(..)
  )
  where

import Lexer
import Data.ByteString.Lazy

data ErrClass
    = Syntactical (Maybe String)
    | Lexical
    | Message String
    deriving (Show, Eq)

data Error = Error
    { errLine  :: Int
    , errPos   :: Int
    , errClass :: ErrClass
    } deriving (Show, Eq)


-- newtype Alex a = Alex { unAlex :: AlexUserState
--                                -> Either String (AlexUserState, a) }


parse :: String -> Either String AlexUserState
parse s = runAlex s $ alexMonadScan >> getUserState
