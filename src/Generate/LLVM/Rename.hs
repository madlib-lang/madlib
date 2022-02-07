{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.Rename where


import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Maybe                    as Maybe
import qualified Data.Bifunctor                as Bifunctor
import qualified Data.ByteString.Lazy.Char8    as BLChar8

import qualified Utils.Hash                    as Hash
import           AST.PostProcessed
import Debug.Trace
import Text.Show.Pretty



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


addHashToName :: String -> String -> String
addHashToName hash name =
  "__" ++ hash ++ "__" ++ name


hashName :: Env -> String -> String
hashName Env { currentModuleHash } =
  addHashToName currentModuleHash


generateHashFromPath :: FilePath -> String
generateHashFromPath =
  Hash.hash . BLChar8.pack


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
  Typed t area (TemplateString exps) ->
    let (renamedExps, env') = renameExps env exps
    in  (Typed t area (TemplateString renamedExps), env')

  Typed t area (Call callType fn args) ->
    let (renamedFn, env')    = renameExp env fn
        (renamedArgs, env'') = renameExps env' args
    in  (Typed t area (Call callType renamedFn renamedArgs), env'')

  Typed t area (Access record field) ->
    let (renamedRecord, env') = renameExp env record
        (renamedField, env'')  = renameExp env' field
    in  (Typed t area (Access renamedRecord renamedField), env'')

  Typed t area (Definition defType params body) ->
    let env'                 = foldr (\param env -> extendScope param param env) env params
        (renamedBody, env'') = renameExps env' body
    in  (Typed t area (Definition defType params renamedBody), env'')

  Typed t area (Assignment name exp) ->
    -- here we deal with an assignment in a body or Do
    -- therefore we must not rename it as it only concerns top level names.
    let env'                = extendScope name name env
        (renamedExp, env'') = renameExp env' exp
    in  (Typed t area (Assignment name renamedExp), env'')

  Typed t area (Var name) -> case break (== '.') name of
    -- A normal name
    (n, []) ->
      let renamed = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
      in  (Typed t area (Var renamed), env)

    -- A field access name eg. Var ".field"
    ([], fieldAccessor) ->
      (Typed t area (Var fieldAccessor), env)

    -- A namespace access eg. Var "List.filter"
    (namespace, '.' : n) ->
      let moduleHash = Maybe.fromMaybe namespace $ Map.lookup namespace (defaultImportHashes env)
          renamed    = addHashToName moduleHash n
          env'       = addDefaultImportNameUsage namespace n env
      in  (Typed t area (Var renamed), env')

    _ ->
      undefined

  Typed t area (NameExport name) ->
    let renamed = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
    in  (Typed t area (NameExport renamed), env)

  Typed t area (TypedExp exp scheme) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area (TypedExp renamedExp scheme), env')

  Typed t area (ListConstructor items) ->
    let (renamedItems, env') = renameListItems env items
    in  (Typed t area (ListConstructor renamedItems), env')

  Typed t area (TupleConstructor items) ->
    let (renamedItems, env') = renameExps env items
    in  (Typed t area (TupleConstructor renamedItems), env')

  Typed t area (Record fields) ->
    let (renamedFields, env') = renameFields env fields
    in  (Typed t area (Record renamedFields), env')

  Typed t area (If cond truthy falsy) ->
    let (renamedCond, env')     = renameExp env cond
        (renamedTruthy, env'')  = renameExp env' truthy
        (renamedFalsy, env''')  = renameExp env'' falsy
    in  (Typed t area (If renamedCond renamedTruthy renamedFalsy), env''')

  Typed t area (Do exps) ->
    let (renamedExps, env') = renameExps env exps
    in  (Typed t area (Do renamedExps), env')

  Typed t area (Where exp iss) ->
    let (renamedExp, env')  = renameExp env exp
        (renamedIss, env'') = renameBranches env' iss
    in  (Typed t area (Where renamedExp renamedIss), env'')

  Typed t area (Placeholder ph exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area (Placeholder ph renamedExp), env')

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
  Typed t area (Is pat exp) ->
    let renamedPattern  = renamePattern env pat
        (renamedExp, env') = renameExp env exp
    in  (Typed t area (Is renamedPattern renamedExp), env')

  _ ->
    undefined


renamePattern :: Env -> Pattern -> Pattern
renamePattern env pat = case pat of
  Typed t area (PCon name args) ->
    let renamed     = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
        renamedArgs = renamePattern env <$> args
    in  Typed t area (PCon renamed renamedArgs)

  Typed t area (PRecord fields) ->
    let renamedFields = Map.map (renamePattern env) fields
    in  Typed t area (PRecord renamedFields)

  Typed t area (PList items) ->
    let renamedItems = renamePattern env <$> items
    in  Typed t area (PList renamedItems)

  Typed t area (PTuple items) ->
    let renamedItems = renamePattern env <$> items
    in  Typed t area (PTuple renamedItems)

  Typed t area (PSpread spread) ->
    let renamedSpread = renamePattern env spread
    in  Typed t area (PSpread renamedSpread)

  other ->
    other


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
  Typed t area (Field (name, exp)) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area $ Field (name, renamedExp), env')

  Typed t area (FieldSpread exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area $ FieldSpread renamedExp, env')

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
  Typed t area (ListItem exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area $ ListItem renamedExp, env')

  Typed t area (ListSpread exp) ->
    let (renamedExp, env') = renameExp env exp
    in  (Typed t area $ ListSpread renamedExp, env')

  _ ->
    undefined


renameTopLevelAssignment :: Env -> Exp -> (Exp, Env)
renameTopLevelAssignment env assignment = case assignment of
  Typed t area (Assignment name exp) ->
    let hashedName          = hashName env name
        env'                = extendScope name hashedName env
        (renamedExp, env'') = renameExp env' exp
    in  (Typed t area (Assignment hashedName renamedExp), env'')

  _ ->
    undefined


renameTopLevelExps :: Env -> [Exp] -> ([Exp], Env)
renameTopLevelExps env exps = case exps of
  (exp : es) -> case exp of
    Typed t area (Assignment _ _) ->
      let (renamedExp, env')  = renameTopLevelAssignment env exp
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (renamedExp : nextExps, nextEnv)

    Typed t area (Export assignment@(Typed _ _ (Assignment _ _))) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area (Export renamedExp) : nextExps, nextEnv)

    Typed t area (TypedExp assignment@(Typed _ _ (Assignment _ _)) scheme) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area (TypedExp renamedExp scheme) : nextExps, nextEnv)

    Typed t area (TypedExp (Typed _ _ (Export assignment@(Typed _ _ (Assignment _ _)))) scheme) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area (TypedExp (Typed t area (Export renamedExp)) scheme) : nextExps, nextEnv)

    Typed t area (Extern qt name foreignName) ->
      let hashedName          = hashName env name
          env'                = extendScope name hashedName env
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area (Extern qt hashedName foreignName) : nextExps, nextEnv)

    Typed _ _ (Export (Typed t area (Extern qt name foreignName))) ->
      let hashedName          = hashName env name
          env'                = extendScope name hashedName env
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Typed t area (Extern qt hashedName foreignName) : nextExps, nextEnv)

    _ ->
      let (exp', env')        = renameExp env exp
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (exp' : nextExps, nextEnv)

  [] ->
    ([], env)



renameConstructor :: Env -> Constructor -> (Constructor, Env)
renameConstructor env constructor = case constructor of
  Untyped area (Constructor name typings t) ->
    let hashedName = hashName env name
        env'       = extendScope name hashedName env
    in  (Untyped area (Constructor hashedName typings t), env')

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
    Untyped area adt@ADT{ adtconstructors } ->
      let (renamedConstructors, env') = renameConstructors env adtconstructors
          (nextTds, nextEnv)          = renameTypeDecls env' tds
      in  (Untyped area adt { adtconstructors = renamedConstructors } : nextTds, nextEnv)

    _ ->
      let (nextTds, nextEnv) = renameTypeDecls env tds
      in  (td : nextTds, nextEnv)

  [] ->
    ([], env)


renameInstance :: Env -> Instance -> (Instance, Env)
renameInstance env inst = case inst of
  Untyped area (Instance name ps p methods) ->
    -- let renamedMethods = Map.map (Bifunctor.first (fst . renameExp env)) methods
    let (renamedMethods, env') =
          Map.foldrWithKey
            (\methodName (method, sc) (renamedMethods, env') ->
              let (renamedMethod, env'') = renameExp env method
              in  ((methodName, (renamedMethod, sc)) : renamedMethods, env'')
            )
            ([], env)
            methods
        renamedMethods' = Map.fromList renamedMethods
    -- let renamedMethods = Map.map (Bifunctor.first (fst . renameExp env)) methods
    in  (Untyped area (Instance name ps p renamedMethods'), env')

  _ ->
    undefined

renameInstances :: Env -> [Instance] -> ([Instance], Env)
renameInstances env instances = case instances of
  (inst : insts) ->
    let (renamed, env') = renameInstance env inst
        (next, env'') = renameInstances env' insts
    in  (renamed : next, env'')

  [] ->
    ([], env)


renamePostProcessedName :: Env -> String -> PostProcessed String -> (PostProcessed String, Env)
renamePostProcessedName env hash solvedName = case solvedName of
  Untyped area name ->
    let hashedName = addHashToName hash name
        env'       = extendScope name hashedName env
    in  (Untyped area hashedName, env')

  _ ->
    undefined


renamePostProcessedNames :: Env -> String -> [PostProcessed String] -> ([PostProcessed String], Env)
renamePostProcessedNames env hash solvedNames = case solvedNames of
  (name : names) ->
    let (renamed, env')        = renamePostProcessedName env hash name
        (nextRenamed, nextEnv) = renamePostProcessedNames env' hash names
    in  (renamed : nextRenamed, nextEnv)

  [] ->
    ([], env)


renameImport :: Env -> Import -> (Import, Env)
renameImport env imp = case imp of
  Untyped area (NamedImport names relPath absPath) ->
    let moduleHash           = generateHashFromPath absPath
        (renamedNames, env') = renamePostProcessedNames env moduleHash names
    in  (Untyped area (NamedImport renamedNames relPath absPath), env')

  {-
    Here we just push the import hash to the env so that we can pull that information to rewrite say:
    Var "List.filter" into Var "__hash_of_List__filter". Later this import will be rewritten as:
    import { __hash_of_List__filter } from "List". Essentially we rewrite all default imports into
    named imports.
  -}
  Untyped area (DefaultImport n@(Untyped _ name) relPath absPath) ->
    let moduleHash = generateHashFromPath absPath
        env'       = addDefaultImportHash name moduleHash env
    in  (Untyped area (DefaultImport n relPath absPath), env')

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
    Typed _ _ (Assignment name _) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ (TypedExp (Typed _ _ (Assignment name _)) _) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ (Export (Typed _ _ (Assignment name _))) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ (TypedExp (Typed _ _ (Export (Typed _ _ (Assignment name _)))) _) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    Typed _ _ (Extern qt name _) ->
      let hashedName = hashName env name
          env'       = extendScope name hashedName env
      in  populateInitialEnv next env'

    _ ->
      populateInitialEnv next env

  [] ->
    env


rewriteDefaultImports :: Env -> [Import] -> [Import]
rewriteDefaultImports env imports = case imports of
  (Untyped area (DefaultImport (Untyped area' namespace) absPath relPath) : next) ->
    let usedNames      = Maybe.fromMaybe Set.empty $ Map.lookup namespace (usedDefaultImportNames env)
        hashFromModule = Maybe.fromMaybe "" $ Map.lookup namespace (defaultImportHashes env)
        hashedNames    = Set.toList $ Set.map (Untyped area' . addHashToName hashFromModule) usedNames
        next'          = rewriteDefaultImports env next
    in  Untyped area (NamedImport hashedNames absPath relPath) : next'

  (imp : next) ->
    imp : rewriteDefaultImports env next

  [] ->
    []


renameAST :: Env -> AST -> (AST, Env)
renameAST env ast =
  let moduleHash                   = hashModulePath ast
      env'                         = populateInitialEnv (aexps ast) env { currentModuleHash = moduleHash }
      (renamedImports, env'')      = renameImports env' $ aimports ast
      (renamedTypeDecls, env''')   = renameTypeDecls env'' $ atypedecls ast
      (renamedExps, env'''')       = renameTopLevelExps env''' $ aexps ast
      (renamedInstances, env''''') = renameInstances env'''' (ainstances ast)
      rewrittenImports             = rewriteDefaultImports env''''' renamedImports
  in  (ast { aexps = renamedExps, atypedecls = renamedTypeDecls, ainstances = renamedInstances, aimports = rewrittenImports }, env)

renameTable :: Table -> Table
renameTable table =
  let env = Env { namesInScope = Map.empty
                , currentModuleHash = ""
                , usedDefaultImportNames = Map.empty
                , defaultImportHashes = Map.empty
                }
  in  Map.map (fst . renameAST env) table
