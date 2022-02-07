module Utils.Types where
import Infer.Type
import qualified Data.Map as Map
import qualified Data.List as List

buildTypeStrForPlaceholder :: [Type] -> String
buildTypeStrForPlaceholder ts = List.intercalate "_" $ getTypeHeadName <$> ts

getTypeHeadName :: Type -> String
getTypeHeadName t = case t of
  TVar (TV n _)   ->
    n

  TCon (TC n _) _ -> case n of
    "{}" ->
      "Unit"

    "(,)" ->
      "Tuple_2"

    "(,,)" ->
      "Tuple_3"

    "(,,,)" ->
      "Tuple_4"

    "(,,,,)" ->
      "Tuple_5"

    "(,,,,,)" ->
      "Tuple_6"

    "(,,,,,,)" ->
      "Tuple_7"

    "(,,,,,,,)" ->
      "Tuple_8"

    "(,,,,,,,,)" ->
      "Tuple_9"

    "(,,,,,,,,,)" ->
      "Tuple_10"

    _ ->
      n

  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    getTypeHeadName tl <> "_arr_" <> getTypeHeadName tr

  TApp l _ ->
    getTypeHeadName l

  TRecord fields _ ->
    let fields'   = Map.map getTypeHeadName fields
        fieldsStr = List.intercalate "_" $ uncurry (++) <$> Map.toList fields'
    in  "Record" <> "_" <> fieldsStr