module Parse.DocString.DocString where

import Data.List (find)
import AST.Source

-- Maybe do this instead:
-- data DocString = ModuleDoc String | Doc String String [DocStringTag] DocStringType
-- Like this we can have more generic functions to extract data from the DocString.

data DocString
  = ModuleDoc SourceTarget String
  | FunctionDoc SourceTarget String String [DocStringTag]
  | TypeDefDoc SourceTarget String String [DocStringTag]
  | InterfaceDoc SourceTarget String String [DocStringTag]
  | InstanceDoc SourceTarget String String [DocStringTag]
  deriving(Eq, Show)

data DocStringTag
  = ExampleTag SourceTarget String
  | SinceTag SourceTarget String
  deriving(Eq, Show)

getTagContent :: DocStringTag -> String
getTagContent tag = case tag of
  ExampleTag _ c ->
    c

  SinceTag _ c ->
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



isModuleDocString :: DocString -> Bool
isModuleDocString ds = case ds of
  ModuleDoc _ _ -> True
  _ -> False

getModuleDocDescription :: DocString -> String
getModuleDocDescription (ModuleDoc _ desc) = desc
