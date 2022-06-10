module Utils.Types where

import           Infer.Type
import qualified Data.Map         as Map
import qualified Data.List        as List
import qualified Utils.Hash       as Hash
import qualified Data.ByteString.Lazy.Char8    as BLChar8


generateHashFromPath :: FilePath -> String
generateHashFromPath =
  Hash.hash . BLChar8.pack


buildTypeStrForPlaceholder :: [Type] -> String
buildTypeStrForPlaceholder ts = List.intercalate "_" $ getTypeHeadName <$> ts


getTypeHeadName :: Type -> String
getTypeHeadName t = case t of
  TVar (TV n _)   ->
    n

  TCon (TC n _) path -> case n of
    "{}" ->
      "Unit_" <> generateHashFromPath path

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
      n <> "_" <> generateHashFromPath path

  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    getTypeHeadName tl <> "_arr_" <> getTypeHeadName tr

  TApp l _ ->
    getTypeHeadName l

  TRecord fields _ ->
    let fields'   = Map.map getTypeHeadName fields
        fieldsStr = List.intercalate "_" $ uncurry (++) <$> Map.toList fields'
    in  "Record" <> "_" <> fieldsStr
