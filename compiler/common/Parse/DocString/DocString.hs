{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Parse.DocString.DocString where

import           Data.List (find)
import           AST.Source
import           Data.Hashable
import           GHC.Generics

-- Maybe do this instead:
-- data DocString = ModuleDoc String | Doc String String [DocStringTag] DocStringType
-- Like this we can have more generic functions to extract data from the DocString.

data DocString
  = ModuleDoc SourceTarget String
  | FunctionDoc SourceTarget String String [DocStringTag]
  | TypeDefDoc SourceTarget String String [DocStringTag]
  | InterfaceDoc SourceTarget String String [DocStringTag]
  | InstanceDoc SourceTarget String String [DocStringTag]
  deriving(Eq, Show, Generic, Hashable)

data DocStringTag
  = ExampleTag SourceTarget String
  | SinceTag SourceTarget String
  | ParamTag SourceTarget String String   -- ^ parameter name, description
  | ReturnsTag SourceTarget String        -- ^ description
  | DeprecatedTag SourceTarget String     -- ^ message
  deriving(Eq, Show, Generic, Hashable)

getTagContent :: DocStringTag -> String
getTagContent tag = case tag of
  ExampleTag _ c ->
    c

  SinceTag _ c ->
    c

  ParamTag _ _ c ->
    c

  ReturnsTag _ c ->
    c

  DeprecatedTag _ c ->
    c

isExampleTag :: DocStringTag -> Bool
isExampleTag tag = case tag of
  ExampleTag _ _ ->
    True

  _ ->
    False

findExampleTag :: [DocStringTag] -> Maybe String
findExampleTag tags =
  let found = find isExampleTag tags
  in  getTagContent <$> found

isSinceTag :: DocStringTag -> Bool
isSinceTag tag = case tag of
  SinceTag _ _ ->
    True

  _ ->
    False

findSinceTag :: [DocStringTag] -> Maybe String
findSinceTag tags =
  let found = find isSinceTag tags
  in  getTagContent <$> found

isParamTag :: DocStringTag -> Bool
isParamTag tag = case tag of
  ParamTag _ _ _ -> True
  _              -> False

findParamTags :: [DocStringTag] -> [(String, String)]
findParamTags tags =
  [ (name, desc) | ParamTag _ name desc <- tags ]

isReturnsTag :: DocStringTag -> Bool
isReturnsTag tag = case tag of
  ReturnsTag _ _ -> True
  _              -> False

findReturnsTag :: [DocStringTag] -> Maybe String
findReturnsTag tags =
  case find isReturnsTag tags of
    Just (ReturnsTag _ c) -> Just c
    _                     -> Nothing

isDeprecatedTag :: DocStringTag -> Bool
isDeprecatedTag tag = case tag of
  DeprecatedTag _ _ -> True
  _                 -> False

findDeprecatedTag :: [DocStringTag] -> Maybe String
findDeprecatedTag tags =
  case find isDeprecatedTag tags of
    Just (DeprecatedTag _ c) -> Just c
    _                        -> Nothing



isModuleDocString :: DocString -> Bool
isModuleDocString ds = case ds of
  ModuleDoc _ _ -> True
  _ -> False

getModuleDocDescription :: DocString -> String
getModuleDocDescription (ModuleDoc _ desc) = desc

getDocStringTarget :: DocString -> SourceTarget
getDocStringTarget docString = case docString of
  ModuleDoc sourceTarget _ ->
    sourceTarget

  FunctionDoc sourceTarget _ _ _ ->
    sourceTarget

  TypeDefDoc sourceTarget _ _ _ ->
    sourceTarget

  InterfaceDoc sourceTarget _ _ _ ->
    sourceTarget

  InstanceDoc sourceTarget _ _ _ ->
    sourceTarget
