{-# LANGUAGE DeriveGeneric #-}
module Parse.Megaparsec.Error where

import           Data.Void
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Set                       as Set
import           Text.Megaparsec

import           Explain.Location


data CustomError
  = BadEscapeSequence Area
  | EmptyCharLiteral Area
  deriving (Eq, Ord, Show)


instance ShowErrorComponent CustomError where
  showErrorComponent (BadEscapeSequence area) =
    "Invalid escape sequence at " ++ show area

  showErrorComponent (EmptyCharLiteral area) =
    "Empty character literal at " ++ show area
