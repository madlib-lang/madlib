{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.Rename where


import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Bifunctor                as Bifunctor
import qualified Data.ByteString.Lazy.Char8    as BLChar8

import           AST.Solved
import qualified Utils.Hash                    as Hash

import           Text.Show.Pretty
import           Debug.Trace


data Env
  = Env
    { namesInScope :: Map.Map String String
    , currentModuleHash :: String
    }
    deriving(Eq, Show)


extendScope :: String -> String -> Env -> Env
extendScope initialName newName env@Env{ namesInScope } =
  env { namesInScope = Map.insert initialName newName namesInScope }


hashName :: Env -> String -> String
hashName Env { currentModuleHash } s =
  "__" ++ currentModuleHash ++ "__" ++ s


hashModulePath :: AST -> String
hashModulePath ast =
  Hash.hash $ BLChar8.pack $ Maybe.fromMaybe "" (apath ast)


renameExpsInBody :: Env -> [Exp] -> ([Exp], Env)
renameExpsInBody env exps = case exps of
  (exp : es) ->
    let (exp', env')        = renameExp env exp
        (nextExps, nextEnv) = renameExpsInBody env' es
    in  (exp' : nextExps, nextEnv)

  [] ->
    ([], env)


renameExp :: Env -> Exp -> (Exp, Env)
renameExp env what = case what of
  Solved t area (TemplateString exps) ->
    let renamedExps = fst . renameExp env <$> exps
    in  (Solved t area (TemplateString renamedExps), env)

  Solved t area (App fn arg isFinal) ->
    let (renamedFn, _)  = renameExp env fn
        (renamedArg, _) = renameExp env arg
    in  (Solved t area (App renamedFn renamedArg isFinal), env)

  Solved t area (Access record field) ->
    let (renamedRecord, _) = renameExp env record
        (renamedField, _)  = renameExp env field
    in  (Solved t area (Access renamedRecord renamedField), env)

  Solved t area (Abs (Solved paramType paramArea paramName) body) ->
    let env'             = extendScope paramName paramName env
        (renamedBody, _) = renameExpsInBody env' body
    in  (Solved t area (Abs (Solved paramType paramArea paramName) renamedBody), env)

  Solved t area (Assignment name exp) ->
    -- here we deal with an assignment in a body or Do
    -- therefore we must not rename it as it only concerns top level names.
    let env'            = extendScope name name env
        (renamedExp, _) = renameExp env exp
    in  (Solved t area (Assignment name renamedExp), env')

  Solved t area (Var name) ->
    let renamed = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
    in  (Solved t area (Var renamed), env)

  Solved t area (NameExport name) ->
    let renamed = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
    in  (Solved t area (NameExport renamed), env)

  Solved t area (TypedExp exp typing scheme) ->
    let (renamedExp, _) = renameExp env exp
    in  (Solved t area (TypedExp renamedExp typing scheme), env)

  Solved t area (ListConstructor items) ->
    let renamedItems = renameListItem env <$> items
    in  (Solved t area (ListConstructor renamedItems), env)

  Solved t area (TupleConstructor items) ->
    let renamedItems = fst . renameExp env <$> items
    in  (Solved t area (TupleConstructor renamedItems), env)

  Solved t area (Record fields) ->
    let renamedFields = renameField env <$> fields
    in  (Solved t area (Record renamedFields), env)

  Solved t area (If cond truthy falsy) ->
    let (renamedCond, _)   = renameExp env cond
        (renamedTruthy, _) = renameExp env truthy
        (renamedFalsy, _)  = renameExp env falsy
    in  (Solved t area (If renamedCond renamedTruthy renamedFalsy), env)

  Solved t area (Do exps) ->
    let (renamedExps, _) = renameExpsInBody env exps
    in  (Solved t area (Do renamedExps), env)

  Solved t area (Where exp iss) ->
    let (renamedExp, _) = renameExp env exp
        renamedIss      = renameBranch env <$> iss
    in  (Solved t area (Where renamedExp renamedIss), env)

  Solved t area (Placeholder ph exp) ->
    let (renamedExp, _) = renameExp env exp
    in  (Solved t area (Placeholder ph renamedExp), env)

  _ ->
    (what, env)


renameBranch :: Env -> Is -> Is
renameBranch env is = case is of
  Solved t area (Is pat exp) ->
    let renamedPattern  = renamePattern env pat
        (renamedExp, _) = renameExp env exp
    in  Solved t area (Is renamedPattern renamedExp)

  _ ->
    undefined


renamePattern :: Env -> Pattern -> Pattern
renamePattern env pat = case pat of
  Solved t area (PCon name args) ->
    let renamed     = Maybe.fromMaybe name $ Map.lookup name (namesInScope env)
        renamedArgs = renamePattern env <$> args
    in  Solved t area (PCon renamed renamedArgs)

  Solved t area (PRecord fields) ->
    let renamedFields = Map.map (renamePattern env) fields
    in  Solved t area (PRecord renamedFields)

  Solved t area (PList items) ->
    let renamedItems = renamePattern env <$> items
    in  Solved t area (PList renamedItems)

  Solved t area (PTuple items) ->
    let renamedItems = renamePattern env <$> items
    in  Solved t area (PTuple renamedItems)

  Solved t area (PSpread spread) ->
    let renamedSpread = renamePattern env spread
    in  Solved t area (PSpread renamedSpread)

  other ->
    other


renameField :: Env -> Field -> Field
renameField env field = case field of
  Solved t area (Field (name, exp)) ->
    let (renamedExp, _) = renameExp env exp
    in  Solved t area $ Field (name, renamedExp)

  Solved t area (FieldSpread exp) ->
    let (renamedExp, _) = renameExp env exp
    in  Solved t area $ FieldSpread renamedExp

  _ ->
    undefined


renameListItem :: Env -> ListItem -> ListItem
renameListItem env item = case item of
  Solved t area (ListItem exp) ->
    let (renamedExp, _) = renameExp env exp
    in  Solved t area $ ListItem renamedExp

  Solved t area (ListSpread exp) ->
    let (renamedExp, _) = renameExp env exp
    in  Solved t area $ ListSpread renamedExp

  _ ->
    undefined


renameTopLevelAssignment :: Env -> Exp -> (Exp, Env)
renameTopLevelAssignment env assignment = case assignment of
  Solved t area (Assignment name exp) ->
    let hashedName      = hashName env name
        env'            = extendScope name hashedName env
        (renamedExp, _) = renameExp env' exp
    in  (Solved t area (Assignment hashedName renamedExp), env')

  _ ->
    undefined


renameTopLevelExps :: Env -> [Exp] -> ([Exp], Env)
renameTopLevelExps env exps = case exps of
  (exp : es) -> case exp of
    Solved t area (Assignment _ _) ->
      let (renamedExp, env')  = renameTopLevelAssignment env exp
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (renamedExp : nextExps, nextEnv)
    
    Solved t area (Export assignment@(Solved _ _ (Assignment _ _))) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Solved t area (Export renamedExp) : nextExps, nextEnv)

    Solved t area (TypedExp assignment@(Solved _ _ (Assignment _ _)) typing scheme) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Solved t area (TypedExp renamedExp typing scheme) : nextExps, nextEnv)

    Solved t area (TypedExp (Solved _ _ (Export assignment@(Solved _ _ (Assignment _ _)))) typing scheme) ->
      let (renamedExp, env')  = renameTopLevelAssignment env assignment
          (nextExps, nextEnv) = renameTopLevelExps env' es
      in  (Solved t area (TypedExp (Solved t area (Export renamedExp)) typing scheme) : nextExps, nextEnv)

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


renameInstance :: Env -> Instance -> Instance
renameInstance env inst = case inst of
  Untyped area (Instance name ps p methods) ->
    let renamedMethods = Map.map (Bifunctor.first (fst . renameExp env)) methods
    in  Untyped area (Instance name ps p renamedMethods)

  _ ->
    undefined


renameAST :: Env -> AST -> (AST, Env)
renameAST env ast =
  let moduleHash                = hashModulePath ast
      env'                      = env { currentModuleHash = moduleHash }
      (renamedTypeDecls, env'') = renameTypeDecls env' $ atypedecls ast
      (renamedExps, env''')     = renameTopLevelExps env'' $ aexps ast
      renamedInstances          = renameInstance env''' <$> ainstances ast
  in  (ast { aexps = renamedExps, atypedecls = renamedTypeDecls, ainstances = renamedInstances }, env)
  -- TODO: we need to generate hashed names for imports that are to be used for the
  -- generation of externs in LLVM phase
--   imports    <- mapM (optimize env') $ Slv.aimports ast
--   exps       <- mapM (optimize env') $ Slv.aexps ast
--   typeDecls  <- mapM (optimize env') $ Slv.atypedecls ast
--   interfaces <- mapM (optimize env') $ Slv.ainterfaces ast
--   instances  <- mapM (optimize env') $ Slv.ainstances ast
  
--   defs <- getTopLevelExps
--   resetTopLevelExps
  
--   return $ Opt.AST { Opt.aimports    = imports
--                     , Opt.aexps       = defs ++ exps
--                     , Opt.atypedecls  = typeDecls
--                     , Opt.ainterfaces = interfaces
--                     , Opt.ainstances  = instances
--                     , Opt.apath       = Slv.apath ast
--                     }


renameTable :: Table -> Table
renameTable table =
  let env       = Env { namesInScope = Map.empty, currentModuleHash = "" }
  in  Map.map (fst . renameAST env) table
