{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.Rename where


import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Maybe                    as Maybe
import qualified Data.Bifunctor                as Bifunctor
import qualified Data.ByteString.Lazy.Char8    as BLChar8

import qualified Utils.Hash                    as Hash
import           AST.Core
import Utils.Hash (generateHashFromPath, addHashToName)
import Debug.Trace
import Text.Show.Pretty (ppShow)



data Env
  = Env
    { namesInScope :: Map.Map String String
    , currentModuleHash :: String
    , usedDefaultImportNames :: Map.Map String (Set.Set String)
    -- ^ Name of the default import as key set of used names as value, eg. List.filter coming from import List from "List"
    , defaultImportHashes :: Map.Map String String
    -- ^ Keys are the imported namespace eg. import List from "List", and the value is the hash of the module
    }
    deriving(Eq, Show)


extendScope :: String -> String -> Env -> Env
extendScope initialName newName env@Env{ namesInScope } =
  env { namesInScope = Map.insert initialName newName namesInScope }


addDefaultImportHash :: String -> String -> Env -> Env
addDefaultImportHash defaultImportName moduleHash env@Env{ defaultImportHashes } =
  env { defaultImportHashes = Map.insert defaultImportName moduleHash defaultImportHashes }

addDefaultImportNameUsage :: String -> String -> Env -> Env
addDefaultImportNameUsage defaultImportName objectName env@Env{ usedDefaultImportNames } =
  let setForName = Maybe.fromMaybe Set.empty $ Map.lookup defaultImportName usedDefaultImportNames
      extended   = Set.insert objectName setForName
  in  env { usedDefaultImportNames = Map.insert defaultImportName extended usedDefaultImportNames }


hashName :: Env -> String -> String
hashName Env { currentModuleHash } =
  addHashToName currentModuleHash


hashModulePath :: AST -> String
hashModulePath ast =
  generateHashFromPath $ Maybe.fromMaybe "" (apath ast)


renameExps :: Env -> [Exp] -> ([Exp], Env)
renameExps env exps = case exps of
  (exp : es) ->
    let (exp', env')        = renameExp env exp
        (nextExps, nextEnv) = renameExps env' es
    in  (exp' : nextExps, nextEnv)

  [] ->
    ([], env)


renameExp :: Env -> Exp -> (Exp, Env)
renameExp env what = case what of
  Typed t area metadata (Call fn args) ->
    let (renamedFn, env')    = renameExp env fn
        (renamedArgs, env'') = renameExps env' args
    in  (Typed t area metadata (Call renamedFn renamedArgs), env'')

  Typed t area metadata (Access record field) ->
    let (renamedRecord, env') = renameExp env record
        (renamedField, env'')  = renameExp env' field
    in  (Typed t area metadata (Access renamedRecord renamedField), env'')

  Typed t area metadata (Definition params body) ->
    let env'                 = foldr (\param env -> extendScope param param env) env (getValue <$> params)
        (renamedBody, env'') = renameExps env' body
    in  (Typed t area metadata (Definition params renamedBody), env'')

  Typed t area metadata (Assignment lhs exp) ->
    -- here we deal with an assignment in a body or Do
    -- therefore we must not rename it as it only concerns top level names.
    case lhs of
      Typed _ _ _ (Var name _) ->
        let env'                = extendScope name name env
            (renamedLhs, env'') = renameExp env' lhs
            (renamedExp, env''') = renameExp env'' exp
        in  (Typed t area metadata (Assignment renamedLhs renamedExp), env''')

      _ ->
        let (renamedLhs, env') = renameExp env lhs
            (renamedExp, env'') = renameExp env' exp
        in  (Typed t area metadata (Assignment renamedLhs renamedExp), env'')

  Typed t area metadata (Var name isConstructor) -> case break (== '.') name of
    -- A normal name
    (_, []) ->
      let renamed = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
      in  (Typed t area metadata (Var renamed isConstructor), env)

    -- A field access name eg. Var ".field"
    ([], fieldAccessor) ->
      (Typed t area metadata (Var fieldAccessor False), env)

    -- A namespace access eg. Var "List.filter"
    (namespace, '.' : n) ->
      let moduleHash = Maybe.fromMaybe namespace $ Map.lookup namespace (defaultImportHashes env)
          renamed    = addHashToName moduleHash n
          env'       = addDefaultImportNameUsage namespace n env
      in  (Typed t area metadata (Var renamed False), env')

    _ ->
      undefined

  Typed t area metadata (NameExport name) ->
    let renamed = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
    in  (Typed t area metadata (NameExport renamed), env)

  Typed t area metadata (ListConstructor items) ->
    let (renamedItems, env') = renameListItems env items
    in  (Typed t area metadata (ListConstructor renamedItems), env')

  Typed t area metadata (TupleConstructor items) ->
    let (renamedItems, env') = renameExps env items
    in  (Typed t area metadata (TupleConstructor renamedItems), env')

  Typed t area metadata (Record fields) ->
    let (renamedFields, env') = renameFields env fields
    in  (Typed t area metadata (Record renamedFields), env')

  Typed t area metadata (If cond truthy falsy) ->
    let (renamedCond, env')     = renameExp env cond
        (renamedTruthy, env'')  = renameExp env' truthy
        (renamedFalsy, env''')  = renameExp env'' falsy
    in  (Typed t area metadata (If renamedCond renamedTruthy renamedFalsy), env''')

  Typed t area metadata (While cond body) ->
    let (renamedCond, env')     = renameExp env cond
        (renamedBody, env'')  = renameExp env' body
    in  (Typed t area metadata (While renamedCond renamedBody), env'')

  Typed t area metadata (Do exps) ->
    let (renamedExps, env') = renameExps env exps
    in  (Typed t area metadata (Do renamedExps), env')

  Typed t area metadata (Where exp iss) ->
    let (renamedExp, env')  = renameExp env exp
        (renamedIss, env'') = renameBranches env' iss
    in  (Typed t area metadata (Where renamedExp renamedIss), env'')

  _ ->
    (what, env)


renameBranches :: Env -> [Is] -> ([Is], Env)
renameBranches env branches = case branches of
  (branch : next) ->
    let (renamed, env') = renameBranch env branch
        (next', env'')  = renameBranches env' next
    in  (renamed : next', env'')

  [] ->
    ([], env)


renameBranch :: Env -> Is -> (Is, Env)
renameBranch env is = case is of
  Typed t area metadata (Is pat exp) ->
    let (renamedPattern, _) = renamePattern env pat
        (renamedExp, env'')    = renameExp env exp
    in  (Typed t area metadata (Is renamedPattern renamedExp), env'')

  _ ->
    undefined


renamePatterns :: Env -> [Pattern] -> ([Pattern], Env)
renamePatterns env patterns = case patterns of
  (pat : next) ->
    let (renamed, env') = renamePattern env pat
        (next', env'')  = renamePatterns env' next
    in  (renamed : next', env'')

  [] ->
    ([], env)

renamePattern :: Env -> Pattern -> (Pattern, Env)
renamePattern env pat = case pat of
  Typed t area metadata (PCon name args) -> case break (== '.') name of
    (namespace, '.' : n) ->
      let moduleHash          = Maybe.fromMaybe namespace $ Map.lookup namespace (defaultImportHashes env)
          (renamedArgs, env') = renamePatterns env args
          renamed             = addHashToName moduleHash n
          env''               = addDefaultImportNameUsage namespace n env'
      in  (Typed t area metadata (PCon renamed renamedArgs), env'')

    _ ->
      let renamed             = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
          (renamedArgs, env') = renamePatterns env args
      in  (Typed t area metadata (PCon renamed renamedArgs), env')

  Typed t area metadata (PRecord fields) ->
    let fieldsAsList = Map.toList fields
        fieldNames = fst <$> fieldsAsList
        fieldPats = snd <$> fieldsAsList
        (renamedPats, env') = renamePatterns env fieldPats
    in  (Typed t area metadata (PRecord (Map.fromList (zip fieldNames renamedPats))), env')

  Typed t area metadata (PList items) ->
    let (renamedItems, env') = renamePatterns env items
    in  (Typed t area metadata (PList renamedItems), env')

  Typed t area metadata (PTuple items) ->
    let (renamedItems, env') = renamePatterns env items
    in  (Typed t area metadata (PTuple renamedItems), env')

  Typed t area metadata (PSpread spread) ->
    let (renamedSpread, env') = renamePattern env spread
    in  (Typed t area metadata (PSpread renamedSpread), env')

  other ->
    (other, env)


renameFields :: Env -> [Field] -> ([Field], Env)
renameFields env fields = case fields of
  (field : next) ->
    let (renamed, env') = renameField env field
        (next', env'')  = renameFields env' next
    in  (renamed : next', env'')

  [] ->
    ([], env)


renameField :: Env -> Field -> (Field, Env)
renameField env field = case field of
  Typed t area metadata (Field (name, exp)) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area metadata $ Field (name, renamedExp), env')

  Typed t area metadata (FieldSpread exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area metadata $ FieldSpread renamedExp, env')

  _ ->
    undefined


renameListItems :: Env -> [ListItem] -> ([ListItem], Env)
renameListItems env lis = case lis of
  (li : next) ->
    let (renamed, env') = renameListItem env li
        (next', env'')  = renameListItems env' next
    in  (renamed : next', env'')

  [] ->
    ([], env)

renameListItem :: Env -> ListItem -> (ListItem, Env)
renameListItem env item = case item of
  Typed t area metadata (ListItem exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area metadata $ ListItem renamedExp, env')

  Typed t area metadata (ListSpread exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area metadata $ ListSpread renamedExp, env')

  _ ->
    undefined


renameTopLevelAssignment :: Env -> Exp -> (Exp, Env)
renameTopLevelAssignment env assignment = case assignment of
  Typed t area metadata (Assignment (Typed lhsT lhsArea lhsMetadata (Var name isCtor)) exp) ->
    let hashedName          = hashName env name
        env'                = extendScope name hashedName env
        (renamedExp, env'') = renameExp env' exp
    in  (Typed t area metadata (Assignment (Typed lhsT lhsArea lhsMetadata (Var hashedName isCtor)) renamedExp), env'')

  _ ->
    undefined


renameTopLevelExps :: Env -> [Exp] -> ([Exp], Env)
renameTopLevelExps env exps = case exps of
  (exp : es) -> case exp of
    Typed _ _ _ (Assignment _ _) ->
      let (renamedExp, env')  = renameTopLevelAssignment env exp
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (renamedExp : nextExps, nextEnv)

    Typed t area metadata (Export assignment@(Typed _ _ _ (Assignment _ _))) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area metadata (Export renamedExp) : nextExps, nextEnv)

    Typed t area metadata (Extern qt name foreignName) ->
      let hashedName          = hashName env name
          env'                = extendScope name hashedName env
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area metadata (Extern qt hashedName foreignName) : nextExps, nextEnv)

    Typed _ _ _ (Export (Typed t area metadata (Extern qt name foreignName))) ->
      let hashedName          = hashName env name
          env'                = extendScope name hashedName env
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area metadata (Extern qt hashedName foreignName) : nextExps, nextEnv)

    _ ->
      let (exp', env')        = renameExp env exp
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (exp' : nextExps, nextEnv)

  [] ->
    ([], env)



renameConstructor :: Env -> Constructor -> (Constructor, Env)
renameConstructor env constructor = case constructor of
  Untyped area metadata (Constructor name typings t) ->
    let hashedName = hashName env name
        env'       = extendScope name hashedName env
    in  (Untyped area metadata (Constructor hashedName typings t), env')

  _ ->
    undefined


renameConstructors :: Env -> [Constructor] -> ([Constructor], Env)
renameConstructors env constructors = case constructors of
  (ctor : ctors) ->
    let (ctor', env')   = renameConstructor env ctor
        (ctors', env'') = renameConstructors env' ctors
    in  (ctor' : ctors', env'')

  [] ->
    ([], env)


renameTypeDecls :: Env -> [TypeDecl] -> ([TypeDecl], Env)
renameTypeDecls env typeDecls = case typeDecls of
  (td : tds) -> case td of
    Untyped area metadata adt@ADT{ adtconstructors } ->
      let (renamedConstructors, env') = renameConstructors env adtconstructors
          (nextTds, nextEnv)          = renameTypeDecls env' tds
      in  (Untyped area metadata adt { adtconstructors = renamedConstructors } : nextTds, nextEnv)

    _ ->
      let (nextTds, nextEnv) = renameTypeDecls env tds
      in  (td : nextTds, nextEnv)

  [] ->
    ([], env)


renamePostProcessedName :: Env -> String -> Core ImportInfo -> (Core ImportInfo, Env)
renamePostProcessedName env hash solvedName = case solvedName of
  Typed qt area metadata (ImportInfo name t) ->
    let hashedName = addHashToName hash name
        env'       = extendScope name hashedName env
    in  (Typed qt area metadata (ImportInfo hashedName t), env')

  _ ->
    undefined


renamePostProcessedNames :: Env -> String -> [Core ImportInfo] -> ([Core ImportInfo], Env)
renamePostProcessedNames env hash solvedNames = case solvedNames of
  (name : names) ->
    let (renamed, env')        = renamePostProcessedName env hash name
        (nextRenamed, nextEnv) = renamePostProcessedNames env' hash names
    in  (renamed : nextRenamed, nextEnv)

  [] ->
    ([], env)


renameImport :: Env -> Import -> (Import, Env)
renameImport env imp = case imp of
  Untyped area metadata (NamedImport names relPath absPath) ->
    let moduleHash           = generateHashFromPath absPath
        (renamedNames, env') = renamePostProcessedNames env moduleHash names
    in  (Untyped area metadata (NamedImport renamedNames relPath absPath), env')

  _ ->
    undefined


renameImports :: Env -> [Import] -> ([Import], Env)
renameImports env imports = case imports of
  (imp : imps) ->
    let (renamedImport, env')  = renameImport env imp
        (nextImports, nextEnv) = renameImports env' imps
    in  (renamedImport : nextImports, nextEnv)

  [] ->
    ([], env)


populateInitialEnv :: [Exp] -> Env -> Env
populateInitialEnv exps env = case exps of
  (exp : next) -> case exp of
    Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) _) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ _ (Export (Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) _))) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ _ (Extern _ name _) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ _ (Export (Typed _ _ _ (Extern _ name _))) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    _ ->
      populateInitialEnv next env

  [] ->
    env


findAlreadyImportedNamesFromModuleWithPath :: [Import] -> FilePath -> [String]
findAlreadyImportedNamesFromModuleWithPath imports path = case imports of
  (Untyped _ _ (NamedImport names _ absPath) : next) ->
    let current =
          if absPath == path then
            getImportName <$> names
          else
            []
    in  current ++ findAlreadyImportedNamesFromModuleWithPath next path

  (_ : next) ->
    findAlreadyImportedNamesFromModuleWithPath next path

  [] ->
    []


dedupeNamedImports :: [String] -> [Import] -> [Import]
dedupeNamedImports alreadyImported imports = case imports of
  (Untyped area metadata (NamedImport names relPath absPath) : next) ->
    -- TODO: we don't need to consider where the import comes from, because it's already hashed and its origin does not matter anymore
    -- as it's already got a unique name.
    let current = Untyped area metadata (NamedImport (filter ((`notElem` alreadyImported) . getImportName) names) relPath absPath)
    in  current : dedupeNamedImports (alreadyImported ++ (getImportName <$> names)) next

  (imp : next) ->
    imp : dedupeNamedImports alreadyImported next

  [] ->
    []



renameAST :: AST -> AST
renameAST ast =
  let env =
        Env { namesInScope = Map.empty
            , currentModuleHash = ""
            , usedDefaultImportNames = Map.empty
            , defaultImportHashes = Map.empty
            }
      moduleHash                   = hashModulePath ast
      env'                         = populateInitialEnv (aexps ast) env { currentModuleHash = moduleHash }
      (renamedImports, env'')      = renameImports env' $ aimports ast
      (renamedTypeDecls, env''')   = renameTypeDecls env'' $ atypedecls ast
      (_, env'''')                 = renameTopLevelExps env''' $ aexps ast

      -- we need a second pass as we may need to rename references above the current top level expression
      (renamedExps, _)             = renameTopLevelExps env'''' $ aexps ast

      dedupedImports               = dedupeNamedImports [] renamedImports
  in  ast { aexps = renamedExps, atypedecls = renamedTypeDecls, aimports = dedupedImports }
