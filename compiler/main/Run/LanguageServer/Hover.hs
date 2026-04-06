{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Run.LanguageServer.Hover
  ( Node(..)
  , getHoverInformation
  , hoverInfoTask
  , nodeToHoverInfo
  , findNodeInAst
  , findNodeAtLoc
  , findNodeInExps
  , findNameInNode
  , isInRange
  , isLocAfter
  , isLocAfterStart
  , failedQt
  , prettyQt
  , prettyScheme
  , sanitizeName
  , internalNames
  , isSyntheticName
  , getNodeLine
  , findForeignAstForName
  , retrieveKind
  ) where

import Language.LSP.Server
import Language.LSP.Types
import qualified Data.Text as T
import qualified Rock
import qualified Driver
import qualified Driver.Query as Query
import qualified Data.Map as Map
import Explain.Location
import qualified Explain.Location as Loc
import           Explain.Format (prettyPrintQualType, prettyPrintType, kindToStr, prettyPrintTyping, prettyPrintTyping', renderSchemesWithDiff)
import           Data.List (foldl')
import qualified Data.List as List
import qualified AST.Solved as Slv
import           Control.Applicative ((<|>))
import           Infer.Type (Qual((:=>)), Type (..), kind, Kind (Star), TCon (..), TVar (..), findTypeVarInType, collectVars, buildKind, getQualified, Scheme (Forall), getParamTypes)
import           Run.Target
import Control.Monad.IO.Class
import qualified AST.Canonical         as Can
import qualified Canonicalize.EnvUtils as CanEnv
import Driver.Query (Query(CanonicalizedASTWithEnv))
import qualified AST.Source as Src
import           Parse.DocString.DocString (DocString(..), DocStringTag(..), findParamTags, findReturnsTag, findDeprecatedTag)
import qualified Canonicalize.Typing as Can
import qualified Infer.Typing as Slv
import qualified Canonicalize.Env as CanEnv
import qualified Canonicalize.Interface as Can
import System.FilePath (takeFileName, dropExtension)
import Run.Options (optEntrypoint)

import Run.LanguageServer.State


isInRange :: Loc -> Area -> Bool
isInRange (Loc _ l c) (Area (Loc _ lstart cstart) (Loc _ lend cend)) =
  (l >= lstart && l <= lend)
  && (not (l == lstart && c < cstart) && not (l == lend && c > cend))


isLocAfter :: Loc -> Area -> Bool
isLocAfter (Loc _ l _) (Area _ (Loc _ lend _)) =
  l > lend

isLocAfterStart :: Loc -> Area -> Bool
isLocAfterStart (Loc _ l _) (Area (Loc _ lstart _) _) =
  l > lstart


failedQt :: Qual Type
failedQt = [] :=> TVar (TV (-1) Star)


prettyQt :: Bool -> Qual Type -> String
prettyQt topLevel qt@(_ :=> t)
  | qt == failedQt = "_"
  | topLevel       = let (r, _) = renderSchemesWithDiff False (Forall [] qt) (Forall [] qt) in r
  | otherwise      = let (r, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t)) in r


prettyScheme :: Scheme -> String
prettyScheme sc =
  let (r, _) = renderSchemesWithDiff False sc sc in r


sanitizeName :: String -> String
sanitizeName s = case s of
  "_P_" ->
    "pipe"

  '_':'_':'$':'P':'H':_ ->
    "$"

  or ->
    or


internalNames :: [String]
internalNames =
  [ "_P_" ]


isSyntheticName :: String -> Bool
isSyntheticName name = "__" `List.isPrefixOf` name


data Node
  = ExpNode Bool Slv.Exp
  | NameNode Bool (Slv.Solved String)
  | PatternNode Slv.Pattern
  | TypingNode (Maybe Slv.Typing) Slv.Typing
  | RecordFieldAnnotation Area String Slv.Typing
  | ADTNode String Area Kind
  | DefaultImportNode Area FilePath
  | NamedImportNode Area String FilePath
  | TypeImportNode Area String FilePath
  deriving(Eq, Show)


getNodeLine :: Node -> Int
getNodeLine n = case n of
  ExpNode _ (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  ExpNode _ (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  NameNode _ (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  NameNode _ (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  PatternNode (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  PatternNode (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  TypingNode _ (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  TypingNode _ (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  ADTNode _ (Area (Loc _ l _) _) _ ->
    l

  RecordFieldAnnotation (Area (Loc _ l _) _) _ _ ->
    l

  DefaultImportNode (Area (Loc _ l _) _) _ ->
    l

  NamedImportNode (Area (Loc _ l _) _) _ _ ->
    l

  TypeImportNode (Area (Loc _ l _) _) _ _ ->
    l


retrieveKind :: Type -> Kind
retrieveKind t = case t of
  TCon (TC _ k) _ _ ->
    k

  TApp l _ ->
    retrieveKind l

  TVar (TV _ k) ->
    k

  _ ->
    kind t


findNodeAtLocInListItem :: Loc -> Slv.ListItem -> Maybe Node
findNodeAtLocInListItem loc li = case li of
  Slv.Typed _ _ (Slv.ListItem exp) ->
    findNodeAtLoc False loc exp

  Slv.Typed _ _ (Slv.ListSpread exp) ->
    findNodeAtLoc False loc exp

  _ ->
    Nothing


findNodeAtLocInField :: Loc -> Slv.Field -> Maybe Node
findNodeAtLocInField loc field = case field of
  Slv.Typed _ _ (Slv.Field (_, exp)) ->
    findNodeAtLoc False loc exp

  Slv.Typed _ _ (Slv.FieldSpread exp) ->
    findNodeAtLoc False loc exp

  _ ->
    Nothing


findNodeAtLocInPattern :: Loc -> Slv.Pattern -> Maybe Node
findNodeAtLocInPattern _ (Slv.Untyped _ _) = Nothing
findNodeAtLocInPattern loc input@(Slv.Typed _ area pat) =
  if isInRange loc area then
    let deeper =
          case pat of
            Slv.PCon _ pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PList pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PTuple pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PRecord pats _ ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PSpread pat ->
              findNodeAtLocInPattern loc pat

            _ ->
              Nothing
    in  case deeper of
          Nothing ->
            Just $ PatternNode input

          Just _ ->
            deeper
  else
    Nothing


findNodeAtLocInIs :: Loc -> Slv.Is -> Maybe Node
findNodeAtLocInIs _ (Slv.Untyped _ _) = Nothing
findNodeAtLocInIs loc (Slv.Typed _ _ (Slv.Is pat exp)) =
  findNodeAtLocInPattern loc pat <|> findNodeAtLoc False loc exp


findNodeInRecordFieldTypeAnnotation :: Loc -> (String, (Area, Slv.Typing)) -> Maybe Node
findNodeInRecordFieldTypeAnnotation loc (fieldName, (area, typing)) =
  if isInRange loc area then
    Just $ RecordFieldAnnotation area fieldName typing
  else
    Nothing


findNodeInTypeAnnotation :: Loc -> Maybe Slv.Typing -> Slv.Typing -> Maybe Node
findNodeInTypeAnnotation loc maybeRoot typing =
  if isInRange loc (Slv.getArea typing) then
    let deeper =
          case Slv.getValue typing of
            Slv.TRComp _ typings ->
              foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc (Just typing) <$> typings

            Slv.TRArr l r ->
              findNodeInTypeAnnotation loc Nothing l <|> findNodeInTypeAnnotation loc Nothing r

            Slv.TRRecord fields _ ->
              foldl' (<|>) Nothing (findNodeInRecordFieldTypeAnnotation loc <$> Map.toList fields)
              <|> foldl' (<|>) Nothing (findNodeInTypeAnnotation loc Nothing <$> Map.elems (snd <$> fields))

            Slv.TRTuple items ->
              foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc Nothing <$> items

            Slv.TRConstrained constraints subTyping ->
              foldl' (<|>) Nothing (findNodeInTypeAnnotation loc Nothing <$> constraints) <|> findNodeInTypeAnnotation loc Nothing subTyping

            _ ->
              Nothing
    in  deeper <|> Just (TypingNode maybeRoot typing)
  else
    Nothing


findNodeAtLoc :: Bool -> Loc -> Slv.Exp -> Maybe Node
findNodeAtLoc topLevel loc input@(Slv.Typed qt area exp) =
  if isInRange loc area then
    let deeper =
          case exp of
            Slv.App fn arg _ ->
              findNodeAtLoc False loc arg <|> findNodeAtLoc False loc fn

            Slv.Abs param@(Slv.Typed _ paramArea _) body ->
              if isInRange loc paramArea then
                Just $ NameNode False param
              else
                foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> body

            Slv.Do body ->
              foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> body

            Slv.Assignment _ exp ->
              findNodeAtLoc False loc exp

            Slv.Mutate lhs exp ->
              findNodeAtLoc False loc lhs <|> findNodeAtLoc False loc exp

            Slv.Access rec field ->
              -- Nothing
              findNodeAtLoc False loc rec <|> findNodeAtLoc False loc field

            Slv.ArrayAccess arr index ->
              findNodeAtLoc False loc arr <|> findNodeAtLoc False loc index

            Slv.If cond truthy falsy ->
              findNodeAtLoc False loc cond
              <|> findNodeAtLoc False loc truthy
              <|> findNodeAtLoc False loc falsy

            Slv.While cond body ->
              findNodeAtLoc False loc cond
              <|> findNodeAtLoc False loc body

            Slv.Export exp' ->
              findNodeAtLoc True loc exp'

            Slv.TypedExp exp' typing _ ->
              findNodeInTypeAnnotation loc Nothing typing
              <|> findNodeAtLoc False loc exp'

            Slv.Var name _ ->
              Just $ NameNode topLevel (Slv.Typed qt area name)

            Slv.Extern _ name _ ->
              Just $ NameNode topLevel (Slv.Typed qt area name)

            Slv.TemplateString exps ->
              foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> exps

            Slv.TupleConstructor items ->
              foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> items

            Slv.ListConstructor items ->
              foldl' (<|>) Nothing $ findNodeAtLocInListItem loc <$> items

            Slv.Record fields ->
              foldl' (<|>) Nothing $ findNodeAtLocInField loc <$> fields

            Slv.Where exp iss ->
              findNodeAtLoc False loc exp
              <|> foldl' (<|>) Nothing (findNodeAtLocInIs loc <$> iss)

            _ ->
              Nothing
    in  case deeper of
          Nothing ->
            Just $ ExpNode topLevel input

          Just _ ->
            deeper
  else
    Nothing
findNodeAtLoc _ _ (Slv.Untyped _ _) =
  Nothing


findNodeInAst :: Loc -> Src.AST -> Slv.AST -> Maybe Node
findNodeInAst loc srcAst slvAst =
  findNodeInExps loc (Slv.aexps slvAst ++ Slv.getAllMethods slvAst)
  <|> findNodeInTypeDeclarations loc (Slv.atypedecls slvAst)
  <|> findNodeInInstanceHeaders loc (Src.ainstances srcAst)
  <|> findNodeInImports loc (Src.aimports srcAst)


findNodeInImports :: Loc -> [Src.Import] -> Maybe Node
findNodeInImports loc imports =
  foldl' (<|>) Nothing $ findNodeInImport loc <$> imports


findNodeInImport :: Loc -> Src.Import -> Maybe Node
findNodeInImport loc imp =
  if isInRange loc $ Src.getArea imp then
    case imp of
      Src.Source impArea _ (Src.DefaultImport _ _ filepath) ->
        Just $ DefaultImportNode impArea filepath

      Src.Source impArea _ (Src.NamedImport names _ filepath) ->
        foldl' (<|>) Nothing (findNamedImportNode loc filepath NamedImportNode <$> names)
        <|> Just (DefaultImportNode impArea filepath)

      Src.Source impArea _ (Src.TypeImport typeNames _ filepath) ->
        foldl' (<|>) Nothing (findNamedImportNode loc filepath TypeImportNode <$> typeNames)
        <|> Just (DefaultImportNode impArea filepath)
  else
    Nothing

findNamedImportNode :: Loc -> FilePath -> (Area -> String -> FilePath -> Node) -> Src.Source Src.Name -> Maybe Node
findNamedImportNode loc importPath ctor (Src.Source area _ n) =
  if isInRange loc area then
    Just $ ctor area n importPath
  else
    Nothing


findNodeInTypeDeclarations :: Loc -> [Slv.TypeDecl] -> Maybe Node
findNodeInTypeDeclarations loc typeDecls = case typeDecls of
  typeDeclaration : next ->
    findNodeInTypeDeclaration loc typeDeclaration
    <|> findNodeInTypeDeclarations loc next

  [] ->
    Nothing


findNodeInConstructor :: Loc -> Slv.Constructor -> Maybe Node
findNodeInConstructor loc constructor =
  if isInRange loc (Slv.getArea constructor) then
    foldl' (<|>) Nothing (findNodeInTypeAnnotation loc Nothing <$> Slv.getConstructorTypings constructor)
    <|> Just (NameNode False (Slv.Typed ([] :=> Slv.getConstructorType constructor) (Slv.getArea constructor) $ Slv.getConstructorName constructor))
  else
    Nothing


findNodeInTypeDeclaration :: Loc -> Slv.TypeDecl -> Maybe Node
findNodeInTypeDeclaration loc typeDeclaration = case typeDeclaration of
  Slv.Untyped area Slv.ADT { Slv.adtname, Slv.adtconstructors, Slv.adtparams } ->
    -- TODO: look in constructors as well and make it a prio as it should be deeper in the AST if found
    if isInRange loc area then
      foldl' (<|>) Nothing (findNodeInConstructor loc <$> adtconstructors)
      <|> Just (ADTNode adtname area (buildKind $ length adtparams))
    else
      Nothing

  Slv.Untyped area Slv.Alias { Slv.aliasname, Slv.aliasparams, Slv.aliastype } ->
    -- TODO: look in typing as well and make it a prio as it should be deeper in the AST if found
    if isInRange loc area then
      findNodeInTypeAnnotation loc Nothing aliastype
      <|> Just (ADTNode aliasname area (buildKind $ length aliasparams))
    else
      Nothing

  _ ->
    Nothing


findNodeInExps :: Loc -> [Slv.Exp] -> Maybe Node
findNodeInExps loc exps = case exps of
  exp : next ->
    findNodeAtLoc True loc exp <|> findNodeInExps loc next

  [] ->
    Nothing


findNodeInInstanceHeaders :: Loc -> [Src.Instance] -> Maybe Node
findNodeInInstanceHeaders loc instances =
  let srcTypings = concatMap (\i -> Src.getInstanceConstraintTypings i ++ Src.getInstanceTypings i) instances
      -- srcTypings = findNodeInExps loc $ Slv.aexps ast ++ Slv.getAllMethods ast
      canTypings = Can.canonicalizeTyping' <$> srcTypings
      slvTypings = Slv.updateTyping <$> canTypings
  in  foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc Nothing <$> slvTypings


findNameInNode :: Maybe Node -> Maybe String
findNameInNode node = case node of
  Just (NameNode _ (Slv.Typed _ _ name)) ->
    Just name

  Just (PatternNode (Slv.Typed _ _ (Slv.PVar name))) ->
    Just name

  Just (PatternNode (Slv.Typed _ _ (Slv.PCon name _))) ->
    Just name

  Just (TypingNode _ (Slv.Untyped _ (Slv.TRComp name _))) ->
    return name

  Just (TypingNode _ (Slv.Untyped _ (Slv.TRSingle name))) ->
    return name

  _ ->
    Nothing


findForeignAstForName :: String -> [Can.Import] -> Maybe FilePath
findForeignAstForName name imports = case imports of
  [] ->
    Nothing

  (Can.Canonical _ (Can.NamedImport names _ path) : _) | name `elem` (Can.getCanonicalContent <$> names) ->
    Just path

  (Can.Canonical _ (Can.DefaultImport namespace _ path) : _) | takeWhile (/= '.') name == Can.getCanonicalContent namespace ->
    Just path

  (Can.Canonical _ (Can.TypeImport names _ path) : _) | name `elem` (Can.getCanonicalContent <$> names) ->
    Just path

  (_ : next) ->
    findForeignAstForName name next


nodeToHoverInfo :: Rock.MonadFetch Query.Query m => FilePath -> Node -> m String
nodeToHoverInfo modulePath node = do
  (_, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
  docStrings <- Rock.fetch $ Query.DocStrings modulePath
  let findDocTags name =
        let found = List.find (\ds -> case ds of { FunctionDoc _ n _ _ -> n == name; _ -> False }) docStrings
        in case found of
          Just (FunctionDoc _ _ _ tags) -> tags
          _                             -> []
  nodeInfo <- case node of
    ExpNode topLevel (Slv.Typed qt _ (Slv.Assignment name _)) ->
      return $ sanitizeName name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _)) ->
      return $ sanitizeName name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _)) ->
      return $ sanitizeName name <> " :: " <> prettyQt topLevel qt

    NameNode topLevel (Slv.Typed qt _ name) -> do
      let prefix =
            if name `elem` internalNames then
              ""
            else
              sanitizeName name <> " :: "
      return $ prefix <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ _) ->
      return $ prettyQt topLevel qt

    PatternNode (Slv.Typed qt _ (Slv.PVar name)) ->
      return $ name <> " :: " <> prettyQt False qt

    PatternNode (Slv.Typed qt _ _) ->
      return $ prettyQt False qt

    ADTNode name _ k ->
      return $ name <> " :: " <> kindToStr k

    RecordFieldAnnotation _ name typing ->
      return $ name <> " :: " <> prettyPrintTyping' False typing

    -- TODO: make dry with case for TRSingle
    TypingNode _ (Slv.Untyped _ (Slv.TRComp name ts)) -> do
      maybeType <- CanEnv.lookupADT' canEnv name
      case maybeType of
        Just t ->
          return $ name <> " :: " <> kindToStr (retrieveKind t)

        Nothing ->
          return $ name <> " :: " <> List.intercalate " -> " (replicate (length ts + 1) "*")

    -- TypingNode maybeRootTyping typing@(Slv.Untyped _ (Slv.TRSingle name)) -> do
    --   case maybeRootTyping of
    --     Just (Slv.Untyped _ (Slv.TRComp rootTypingName typings)) -> do
    --       maybeInterface <- Rock.fetch $ Query.CanonicalizedInterface modulePath rootTypingName
    --       return $ "root: " <> ppShow maybeRootTyping <> "\ntyping: " <> ppShow typing <> "\nmaybe interface: " <> ppShow maybeInterface
    TypingNode maybeRootTyping typing@(Slv.Untyped _ (Slv.TRSingle name)) -> do
      maybeKind <- case maybeRootTyping of
        Just (Slv.Untyped _ (Slv.TRComp rootTypingName typings)) -> do
          -- first we try to find a var in an adt
          maybeRootType <- CanEnv.lookupADT' canEnv rootTypingName
          case maybeRootType of
            Just t -> do
              let allVars = collectVars t
              case findTRSingleIndex typings typing of
                Just i ->
                  if i < length allVars then
                    return $ Just $ kind $ allVars !! i
                  else
                    return Nothing
                _ ->
                  return Nothing

            _ -> do
              -- otherwise we try to find a var in an interface declaration
              -- maybeInterface <- Rock.fetch $ Query.CanonicalizedInterface modulePath rootTypingName
              maybeInterface <- Can.lookupInterface' canEnv rootTypingName
              return $ maybeInterface >>= \(CanEnv.Interface vars _ _) -> do
                case findTRSingleIndex typings typing of
                  Just i ->
                    if i < length vars then
                      Just $ kind $ vars !! i
                    else
                      Nothing

                  _ ->
                    Nothing
            where
              findTRSingleIndex :: [Slv.Typing] -> Slv.Typing -> Maybe Int
              findTRSingleIndex trCompVars trSingle = case trCompVars of
                (trc : _) | trc == trSingle ->
                  Just 0

                (_ : next) ->
                  (+ 1) <$> findTRSingleIndex next trSingle

                _ ->
                  Nothing

        _ ->
          return Nothing

      case maybeKind of
        Just k ->
          return $ name <> " :: " <> kindToStr k

        Nothing -> do
          maybeType <- CanEnv.lookupADT' canEnv name
          case maybeType of
            Just t ->
              return $ name <> " :: " <> kindToStr (retrieveKind t)

            Nothing ->
              return $ name <> " :: *"

    TypingNode _ (Slv.Untyped _ (Slv.TRArr _ _)) ->
      return "(->) :: * -> * -> *"

    TypingNode _ _ ->
      return "*"

    -- TODO: add docstring for the module here
    DefaultImportNode _ filepath ->
      return $ dropExtension (takeFileName filepath)

    NamedImportNode _ name filepath -> do
      maybeExp <- Rock.fetch $ Query.ForeignExp filepath name
      case maybeExp of
        Just exp ->
          let qt = Slv.getQualType exp
          in  return $ name <> " :: " <> prettyQt False qt

        Nothing ->
          return name

    TypeImportNode _ name _ -> do
      maybeType <- CanEnv.lookupADT' canEnv name
      case maybeType of
        Just t ->
          return $ name <> " :: " <> kindToStr (retrieveKind t)

        Nothing ->
          return name

    _ ->
      return ""

  let maybeName = case node of
        ExpNode _ (Slv.Typed _ _ (Slv.Assignment name _)) -> Just name
        ExpNode _ (Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _)) -> Just name
        ExpNode _ (Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _)) -> Just name
        NameNode _ (Slv.Typed _ _ name) -> Just name
        _ -> Nothing
      docTagsSection = case maybeName of
        Just name ->
          let tags       = findDocTags name
              params     = findParamTags tags
              returns    = findReturnsTag tags
              deprecated = findDeprecatedTag tags
              paramLines =
                if null params then ""
                else "\n\n**Parameters:**\n" <> List.intercalate "\n" ((\(n, d) -> "- `" <> n <> "` — " <> d) <$> params)
              returnsLine = case returns of
                Just r  -> "\n\n**Returns:** " <> r
                Nothing -> ""
              deprecatedLine = case deprecated of
                Just d  -> "\n\n⚠️ *Deprecated: " <> d <> "*"
                Nothing -> ""
          in  paramLines <> returnsLine <> deprecatedLine
        Nothing -> ""
  return $
    "```madlib\n"
    <> nodeInfo
    <> "\n"
    <> "```\n\n"
    <> "*Defined in '"
    <> modulePath
    <> "' at line "
    <> show (getNodeLine node)
    <> "*"
    <> docTagsSection


getHoverInformation :: State -> Loc -> FilePath -> LspM () (Maybe String)
getHoverInformation state loc path = do
  jsOptions <- buildOptions TNode
  jsResult <- liftIO $ safeRunTask state jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
  case jsResult of
    Just (Just info, _, _) -> return (Just info)
    _ -> do
      llvmOptions <- buildOptions TLLVM
      llvmResult <- liftIO $ safeRunTask state llvmOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
      return $ case llvmResult of
        Just (r, _, _) -> r
        Nothing        -> Nothing


hoverInfoTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe String)
hoverInfoTask loc path = do
  -- hasCycle <- Rock.fetch $ Query.DetectImportCycle [] path
  -- if hasCycle then
  --   return Nothing
  -- else do
    srcAst        <- Rock.fetch $ Query.ParsedAST path
    (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
    mapM (nodeToHoverInfo path) (findNodeInAst loc srcAst typedAst)
