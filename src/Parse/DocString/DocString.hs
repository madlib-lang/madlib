module Parse.DocString.DocString where

data DocString
  = ModuleDoc String
  | FunctionDoc String String
  deriving(Eq, Show)

isModuleDocString :: DocString -> Bool
isModuleDocString ds = case ds of
  ModuleDoc _ -> True
  _ -> False

getModuleDocDescription :: DocString -> String
getModuleDocDescription (ModuleDoc desc) = desc
