module Parse.DocString.DocString where

import Data.List (find)

-- Maybe do this instead:
-- data DocString = ModuleDoc String | Doc String String [DocStringTag] DocStringType
-- Like this we can have more generic functions to extract data from the DocString.

data DocString
  = ModuleDoc String
  | FunctionDoc String String [DocStringTag]
  | TypeDefDoc String String [DocStringTag]
  | InterfaceDoc String String [DocStringTag]
  | InstanceDoc String String [DocStringTag]
  deriving(Eq, Show)

data DocStringTag
  = ExampleTag String
  | SinceTag String
  deriving(Eq, Show)

getTagContent :: DocStringTag -> String
getTagContent tag = case tag of
  ExampleTag c -> c
  SinceTag   c -> c

isExampleTag :: DocStringTag -> Bool
isExampleTag tag = case tag of
  ExampleTag _ -> True
  _            -> False

findExampleTag :: [DocStringTag] -> Maybe String
findExampleTag tags =
  let found = find isExampleTag tags
  in  getTagContent <$> found

isSinceTag :: DocStringTag -> Bool
isSinceTag tag = case tag of
  SinceTag _ -> True
  _          -> False

findSinceTag :: [DocStringTag] -> Maybe String
findSinceTag tags =
  let found = find isSinceTag tags
  in  getTagContent <$> found



isModuleDocString :: DocString -> Bool
isModuleDocString ds = case ds of
  ModuleDoc _ -> True
  _ -> False

getModuleDocDescription :: DocString -> String
getModuleDocDescription (ModuleDoc desc) = desc
