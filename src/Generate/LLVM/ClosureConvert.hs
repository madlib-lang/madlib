{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.ClosureConvert where

import qualified Control.Monad.State           as MonadState
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.List
import qualified AST.Core                      as Core
import qualified AST.ClosureConverted          as CC
import           Infer.Type
import Data.Maybe
import Debug.Trace
import Text.Show.Pretty
import Explain.Location
import qualified Utils.Types as Types


data State
  = State { count :: Int, topLevel :: [CC.Exp] }

data Env
  = Env
  { freeVars :: [CC.Name]
  , freeVarExclusion :: [CC.Name]
  -- ^ Closured names that are reassigned. So if we find something in the higher scope, the inner function should not skip it but create a param for it
  , stillTopLevel :: Bool
  , lifted :: M.Map String (String, [CC.Exp])
  -- ^ the key is the initial name, and then we have (lifted name, args to partially apply)
  }

initialOptimizationState :: State
initialOptimizationState = State { count = 0, topLevel = [] }

type Convert a = forall m . MonadState.MonadState State m => m a


numbers :: [String]
numbers = show <$> [0 ..]

generateLiftedName :: String -> Convert String
generateLiftedName originalName = do
  s@(State count _) <- MonadState.get
  let index = numbers !! count
  let name = originalName ++ "$lifted$" ++ index
  MonadState.put s { count = count + 1 }
  return name

resetTopLevelExps :: Convert ()
resetTopLevelExps = do
  s@(State _ topLevel) <- MonadState.get
  MonadState.put s { topLevel = [] }

addTopLevelExp :: CC.Exp -> Convert ()
addTopLevelExp exp = do
  s@(State _ topLevel) <- MonadState.get
  MonadState.put s { topLevel = topLevel ++ [exp] }

getTopLevelExps :: Convert [CC.Exp]
getTopLevelExps = do
  State _ topLevel <- MonadState.get
  return topLevel

addGlobalFreeVar :: CC.Name -> Env -> Env
addGlobalFreeVar fv env =
  env { freeVars = fv : freeVars env }

addVarExclusion :: String -> Env -> Env
addVarExclusion var env =
  env { freeVarExclusion = var : freeVarExclusion env }

addVarExclusions :: [String] -> Env -> Env
addVarExclusions vars env =
  env { freeVarExclusion = vars ++ freeVarExclusion env }

addLiftedLambda :: CC.Name -> CC.Name -> [CC.Exp] -> Env -> Env
addLiftedLambda originalName liftedName args env =
  env { lifted = M.insert originalName (liftedName, args) $ lifted env }


findFreeVars :: Env -> Core.Exp -> Convert [(String, CC.Exp)]
findFreeVars env exp = do
  fvs <- case exp of
    Core.Typed _ _ (Core.Var "+") ->
      return []

    Core.Typed _ _ (Core.Var "++") ->
      return []

    Core.Typed _ _ (Core.Var "-") ->
      return []

    Core.Typed _ _ (Core.Var "*") ->
      return []

    Core.Typed _ _ (Core.Var "/") ->
      return []

    Core.Typed _ _ (Core.Var "==") ->
      return []

    Core.Typed _ _ (Core.Var "!=") ->
      return []

    Core.Typed _ _ (Core.Var "!") ->
      return []

    Core.Typed _ _ (Core.Var ">") ->
      return []

    Core.Typed _ _ (Core.Var "<") ->
      return []

    Core.Typed _ _ (Core.Var ">=") ->
      return []

    Core.Typed _ _ (Core.Var "<=") ->
      return []

    Core.Typed _ _ (Core.Var "&&") ->
      return []

    Core.Typed _ _ (Core.Var "%") ->
      return []

    -- field access should not be registered as a free var
    Core.Typed _ _ (Core.Var ('.' : _)) ->
      return []

    Core.Typed _ _ (Core.Var n) -> do
      var' <- convert env exp
      return [(n, var')]

    Core.Typed _ _ (Core.Definition _ params body) -> do
      vars <- findFreeVarsInBody env body
      return $ filter (\(varName, _) -> varName `notElem` params) vars
      where
        findFreeVarsInBody :: Env -> [Core.Exp] -> Convert [(String, CC.Exp)]
        findFreeVarsInBody env exps = case exps of
          (e : es) -> case e of
            Core.Typed _ _ (Core.Assignment name exp) -> do
              fvs     <- findFreeVars (addGlobalFreeVar name env) exp
              nextFVs <- findFreeVarsInBody (addGlobalFreeVar name env) es
              return $ fvs ++ nextFVs

            Core.Typed _ _ (Core.TypedExp (Core.Typed _ _ (Core.Assignment name exp)) _) -> do
              fvs     <- findFreeVars (addGlobalFreeVar name env) exp
              nextFVs <- findFreeVarsInBody (addGlobalFreeVar name env) es
              return $ fvs ++ nextFVs

            _ -> do
              fvs     <- findFreeVars env e
              nextFVs <- findFreeVarsInBody env es
              return $ fvs ++ nextFVs

          [] ->
            return []

    Core.Typed _ _ (Core.Call _ f args) -> do
      fFreeVars   <- findFreeVars env f
      argFreeVars <- concat <$> mapM (findFreeVars env) args
      return $ fFreeVars ++ argFreeVars

    Core.Typed _ _ (Core.Do exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Core.Typed _ _ (Core.If cond truthy falsy) -> do
      condFreeVars   <- findFreeVars env cond
      truthyFreeVars <- findFreeVars env truthy
      falsyFreeVars  <- findFreeVars env falsy
      return $ condFreeVars ++ truthyFreeVars ++ falsyFreeVars

    Core.Typed _ _ (Core.TupleConstructor exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Core.Typed _ _ (Core.Access record field) -> do
      recordVars <- findFreeVars env record
      fieldVars  <- findFreeVars env field
      return $ recordVars ++ fieldVars

    Core.Typed _ _ (Core.ListConstructor exps) -> do
      vars <- mapM (findFreeVars env . Core.getListItemExp) exps
      return $ concat vars

    Core.Typed _ _ (Core.Where whereExp iss) -> do
      expVars     <- findFreeVars env whereExp
      issFreeVars <- findFreeVarsInBranches env iss
      return $ expVars ++ issFreeVars

    Core.Typed _ _ (Core.Assignment n exp) -> do
      findFreeVars env exp

    Core.Typed _ _ (Core.TypedExp exp _) -> do
      findFreeVars env exp

    -- TODO: Check that we still need this
    Core.Typed (_ :=> t) area (Core.LNum x) -> case t of
      TVar _ -> do
        let dictName = "$Number$" <> Types.buildTypeStrForPlaceholder [t]
        if dictName `notElem` freeVars env then
          return [(dictName, CC.Typed ([] :=> tVar "dict") area (CC.Var dictName))]
        else
          return []

      _ ->
        return []

    Core.Typed _ area (Core.Placeholder ph exp) -> do
      (placeholderVars, excludeVars) <- case ph of
        (Core.ClassRef interface _ _ True, ts) ->
          let dictName = "$" <> interface <> "$" <> ts
          in  return ([(dictName, CC.Typed ([] :=> tVar "dict") area (CC.Var dictName))], [])

        (Core.MethodRef interface methodName True, ts) -> do
          let dictName = "$" <> interface <> "$" <> ts
          return ([(dictName, CC.Typed ([] :=> tVar "dict") area (CC.Var dictName))], [methodName])

        _ ->
          return ([], [])
      expVars <- case ph of
        -- If it's a resolved method, it is accessed from the global scope
        (Core.MethodRef _ _ False, _) ->
          return []

        _ ->
          findFreeVars env exp
      return $ filter (\(varName, _) -> varName `notElem` excludeVars) $ placeholderVars ++ expVars

    Core.Typed _ _ (Core.Record fields) -> do
      fvs <- mapM findFreeVarsInField fields
      return $ concat fvs
      where
        findFreeVarsInField :: Core.Field -> Convert [(String, CC.Exp)]
        findFreeVarsInField field = case field of
          Core.Typed _ _ (Core.Field (_, exp)) ->
            findFreeVars env exp

          Core.Typed _ _ (Core.FieldSpread exp) ->
            findFreeVars env exp

    _ ->
      return []

  let globalVars = freeVars env ++ M.keys (lifted env)
  let fvs' = M.toList $ M.fromList fvs

  return $ filter (\(varName, _) -> varName `notElem` globalVars || varName `elem` freeVarExclusion env) fvs'

findFreeVarsInBranches :: Env -> [Core.Is] -> Convert [(String, CC.Exp)]
findFreeVarsInBranches env iss = case iss of
  (is : next) -> do
    branchVars <- findFreeVarsInBranch env is
    nextVars   <- findFreeVarsInBranches env next
    return $ branchVars ++ nextVars

  [] ->
    return []

findFreeVarsInBranch :: Env -> Core.Is -> Convert [(String, CC.Exp)]
findFreeVarsInBranch env is = case is of
  Core.Typed _ _ (Core.Is pat exp) -> do
    let patternVars = getPatternVars pat
    expVars <- findFreeVars env exp
    return $ filter (\(varName, _) -> varName `notElem` patternVars) expVars


getPatternVars :: Core.Pattern -> [String]
getPatternVars (Core.Typed _ _ pat) = case pat of
  Core.PVar n ->
    [n]

  Core.PCon _ pats ->
    concatMap getPatternVars pats

  Core.PRecord fields ->
    concatMap getPatternVars $ M.elems fields

  Core.PList pats ->
    concatMap getPatternVars pats

  Core.PTuple pats ->
    concatMap getPatternVars pats

  Core.PSpread pat' ->
    getPatternVars pat'

  _ ->
    []


class Convertable a b where
  convert :: Env -> a -> Convert b


-- At this point it's no longer top level and all functions encountered must be lifted
convertBody :: [String] -> Env -> [Core.Exp] -> Convert [CC.Exp]
convertBody exclusionVars env body = case body of
  [] ->
    return []

  (exp : es) -> case exp of
    Core.Typed _ _ (Core.TypedExp (Core.Typed qt@(_ :=> t) area (Core.Assignment name abs@(Core.Typed _ _ (Core.Definition _ params body)))) _) -> do
      exp' <- convertAbs (addVarExclusions exclusionVars env) name [] abs
      next <- convertBody (name : exclusionVars) (addGlobalFreeVar name env) es
      return $ exp' : next

    Core.Typed qt@(_ :=> t) area (Core.Assignment name abs@(Core.Typed _ _ (Core.Definition _ params body))) -> do
      exp' <- convertAbs (addVarExclusions exclusionVars env) name [] abs
      next <- convertBody (name : exclusionVars) (addGlobalFreeVar name env) es
      return $ exp' : next

    abs@(Core.Typed _ _ (Core.Definition _ params body)) -> do
      exp' <- convert (addVarExclusions exclusionVars env) abs
      next <- convertBody exclusionVars env es
      return $ exp' : next

    Core.Typed _ _ (Core.TypedExp (Core.Typed _ _ (Core.Assignment name e)) _) -> do
      e'   <- convert env exp
      next <- convertBody (name : exclusionVars) env es
      return $ e' : next

    Core.Typed _ _ (Core.Assignment name e) -> do
      e'   <- convert env exp
      next <- convertBody (name : exclusionVars) env es
      return $ e' : next

    _ -> do
      exp' <- convert env exp
      next <- convertBody exclusionVars env es
      return $ exp' : next


collectPlaceholderParams :: Core.Exp -> ([String], Core.Exp)
collectPlaceholderParams ph = case ph of
  Core.Typed _ _ (Core.Placeholder (Core.ClassRef interfaceName _ False True, ts) next) ->
    let (nextParams, nextBody) = collectPlaceholderParams next
    in  ("$" <> interfaceName <> "$" <> ts : nextParams, nextBody)

  or ->
    ([], or)


convertDefType :: Core.DefinitionType -> CC.DefinitionType
convertDefType defType = case defType of
  Core.BasicDefinition ->
    CC.BasicDefinition

  Core.TCEOptimizableDefinition ->
    CC.TCEOptimizableDefinition

convertCallType :: Core.CallType -> CC.CallType
convertCallType defType = case defType of
  Core.SimpleCall ->
    CC.SimpleCall

  Core.RecursiveTailCall ->
    CC.RecursiveTailCall


convertAbs :: Env -> String -> [String] -> Core.Exp -> Convert CC.Exp
convertAbs env functionName placeholders abs@(Core.Typed (ps :=> t) area (Core.Definition defType params body)) = do
  let isTopLevel = stillTopLevel env
  if isTopLevel then do
    let params' = placeholders ++ params
    body'' <- convertBody [] env { stillTopLevel = False } body

    -- Hacky for now
    let paramTypes = (tVar "dict" <$ placeholders) ++ getParamTypes t
    let t' =
          if length paramTypes < length params' then
            tVar "a" `fn` t
          else
            foldr fn t $ tVar "dict" <$ placeholders

    return $ CC.Typed (ps :=> t') area $ CC.Definition (convertDefType defType) functionName params' body''
  else do
    -- here we need to add free var parameters, lift it, and if there is any free var, replace the abs with a
    -- PAP that applies the free vars from the current scope.
    fvs           <- findFreeVars (addGlobalFreeVar functionName env) abs
    functionName' <- generateLiftedName functionName
    body''        <- convertBody [] (addLiftedLambda functionName functionName' (snd <$> fvs) env) body

    let paramsWithFreeVars = (fst <$> fvs) ++ params

    let liftedType = foldr fn t (CC.getType . snd <$> fvs)
    let lifted = CC.Typed (ps :=> liftedType) area (CC.Definition (convertDefType defType) functionName' paramsWithFreeVars body'')
    addTopLevelExp lifted

    let functionNode = CC.Typed (ps :=> t) area (CC.Var functionName')

    if null fvs then
      return $ CC.Typed (ps :=> t) area (CC.Assignment functionName functionNode False)
    else
      -- We need to carry types here
      let fvVarNodes = snd <$> fvs
      in  return $ CC.Typed (ps :=> t) area (CC.Assignment functionName (CC.Typed (ps :=> t) area (CC.Call CC.SimpleCall functionNode fvVarNodes)) False)


-- When a lifted lambda is fetched from the env via Var, we apply the captured args to it
-- which results in codegen generating a PAP for it and then doing the real call with the
-- explicit args from the source language. With this we contract it back to one single call.
dedupeCallFn :: CC.Exp -> CC.Exp
dedupeCallFn exp = case exp of
  CC.Typed qt area (CC.Call ct fn args) ->
    case fn of
      CC.Typed qt' area' (CC.Call ct' fn' args') ->
        dedupeCallFn $ CC.Typed qt area (CC.Call ct' fn' (args' ++ args))

      _ ->
        exp

  _ ->
    exp


instance Convertable Core.Exp CC.Exp where
  convert _ (Core.Untyped area (Core.TypeExport name)) = return $ CC.Untyped area (CC.TypeExport name)
  convert env fullExp@(Core.Typed qt@(ps :=> t) area e) = case e of
    Core.LNum x -> case t of
      TVar _ ->
        return $ CC.Typed qt area (
          CC.Call
            CC.SimpleCall
            ( CC.Typed qt area (
                CC.Placeholder
                  (CC.MethodRef "Number" "__coerceNumber__" True, Types.buildTypeStrForPlaceholder [t])
                  (CC.Typed qt area (CC.Var "__coerceNumber__"))
              )
            )
            [CC.Typed qt area (CC.LNum x)]
        )

      _ ->
        return $ CC.Typed qt area (CC.LNum x)

    Core.LFloat x ->
      return $ CC.Typed qt area (CC.LFloat x)

    Core.LStr x ->
      return $ CC.Typed qt area (CC.LStr x)

    Core.LBool x ->
      return $ CC.Typed qt area (CC.LBool x)

    Core.LUnit ->
      return $ CC.Typed qt area CC.LUnit

    -- Core.TemplateString es -> do
    --   es' <- mapM (convert env { stillTopLevel = False }) es
    --   return $ CC.Typed qt area (CC.TemplateString es')

    Core.JSExp js         -> return $ CC.Typed qt area (CC.JSExp js)

    Core.Call callType fn args -> do
        fn'   <- convert env { stillTopLevel = False } fn
        args' <- mapM (convert env { stillTopLevel = False }) args
        return $ dedupeCallFn $ CC.Typed qt area (CC.Call (convertCallType callType) fn' args')

    Core.Access rec field -> do
      rec'   <- convert env { stillTopLevel = False } rec
      field' <- convert env { stillTopLevel = False } field
      return $ CC.Typed qt area (CC.Access rec' field')

    Core.Export (Core.Typed _ _ (Core.Assignment name abs@(Core.Typed _ _ (Core.Definition _ params body)))) -> do
      convertAbs env name [] abs

    Core.TypedExp (Core.Typed _ _ (Core.Export (Core.Typed _ _ (Core.Assignment name abs@(Core.Typed _ _ (Core.Definition _ params body)))))) _ -> do
      convertAbs env name [] abs

    Core.TypedExp (Core.Typed _ _ (Core.Assignment name abs@(Core.Typed _ _ (Core.Definition _ params body)))) _ -> do
      convertAbs env name [] abs

    Core.Assignment name abs@(Core.Typed _ _ (Core.Definition _ params body)) -> do
      convertAbs env name [] abs

    -- unnamed abs, we need to generate a name here
    Core.Definition defType params body -> do
      body''       <- convertBody [] env body
      fvs          <- findFreeVars env fullExp
      functionName <- generateLiftedName "$lambda"

      let paramsWithFreeVars = (fst <$> fvs) ++ params

      let liftedType = foldr fn t (CC.getType . snd <$> fvs)
      let lifted = CC.Typed (ps :=> liftedType) area (CC.Definition (convertDefType defType) functionName paramsWithFreeVars body'')
      addTopLevelExp lifted

      let functionNode = CC.Typed (ps :=> liftedType) area (CC.Var functionName)

      if null fvs then
        return functionNode
      else
        let fvVarNodes = snd <$> fvs
        in  return $ CC.Typed qt area (CC.Call CC.SimpleCall functionNode fvVarNodes)

    Core.Assignment functionName ph@(Core.Typed _ _ (Core.Placeholder (placeholderRef@(Core.ClassRef interfaceName _ False _), ts) exp)) -> do
      let isTopLevel = stillTopLevel env
      if isTopLevel then do
        let (params, innerExp)   = collectPlaceholderParams ph
        let typeWithPlaceholders = foldr fn t (tVar "dict" <$ params)
        case innerExp of
          Core.Typed _ _ (Core.Definition _ _ _) -> do
            convertAbs env functionName params innerExp
          _ -> do
            innerExp' <- convert env { stillTopLevel = False } innerExp
            return $ CC.Typed (ps :=> typeWithPlaceholders) area $ CC.Definition CC.BasicDefinition functionName params [innerExp']
      else do
        let (dictParams, innerExp) = collectPlaceholderParams ph
            isFunction = isFunctionType (Core.getType exp)
        let env' =
              -- if the wrapped thing is not a function type, we just have a normal exp wrapped in a function that
              -- takes placeholders, we should then closure the variable as well if it's a reassignment
              if isFunction then
                addGlobalFreeVar functionName env
              else
                env
        fvs <- findFreeVars (addGlobalFreeVar functionName env') innerExp
        let fvsWithoutDictionary = filter (not . (`elem` dictParams) . fst) fvs
        let paramsWithFreeVars   = dictParams ++ (fst <$> fvsWithoutDictionary)

        functionName' <- generateLiftedName functionName
        innerExp'     <- convert (addLiftedLambda functionName functionName' (CC.Typed ([] :=> tVar "dict") emptyArea . CC.Var <$> paramsWithFreeVars) env) innerExp

        let liftedType = foldr fn t ((tVar "dict" <$ dictParams) ++ (CC.getType . snd <$> fvs))
        let lifted = CC.Typed (ps :=> liftedType) area (CC.Definition CC.BasicDefinition functionName' paramsWithFreeVars [innerExp'])
        addTopLevelExp lifted

        let functionNode = CC.Typed (ps :=> liftedType) area (CC.Var functionName')

        return $ CC.Typed qt area (CC.Assignment functionName functionNode False)

    -- TODO: Add top level info so that we can generate or not the name for the global scope
    Core.Assignment name exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area (CC.Assignment name exp' (stillTopLevel env))

    Core.Export exp -> do
      convert env exp

    Core.NameExport name     ->
      return $ CC.Typed qt area (CC.NameExport name)

    Core.Var        name     -> case M.lookup name (lifted env) of
      Just (newName, capturedArgs) ->
        return $ CC.Typed qt area (CC.Call CC.SimpleCall (CC.Typed qt area (CC.Var newName)) capturedArgs)

      Nothing ->
        return $ CC.Typed qt area (CC.Var name)

    Core.TypedExp exp scheme -> do
      exp' <- convert env exp
      return $ CC.Typed qt area (CC.TypedExp exp' scheme)

    Core.ListConstructor items -> do
      items' <- mapM (convert env) items
      return $ CC.Typed qt area (CC.ListConstructor items')

    Core.TupleConstructor exps -> do
      exps' <- mapM (convert env) exps
      return $ CC.Typed qt area (CC.TupleConstructor exps')

    Core.Record fields -> do
      fields' <- mapM (convert env { stillTopLevel = False }) fields
      return $ CC.Typed qt area (CC.Record fields')

    Core.If cond truthy falsy -> do
      cond'   <- convert env { stillTopLevel = False } cond
      truthy' <- convert env { stillTopLevel = False } truthy
      falsy'  <- convert env { stillTopLevel = False } falsy
      return $ CC.Typed qt area (CC.If cond' truthy' falsy')

    Core.Do exps -> do
      exps' <- convertBody [] env { stillTopLevel = False } exps
      return $ CC.Typed qt area (CC.Do exps')

    Core.Where exp iss -> do
      exp' <- convert env { stillTopLevel = False } exp
      iss' <- mapM (convert env { stillTopLevel = False }) iss
      return $ CC.Typed qt area (CC.Where exp' iss')

    Core.Extern qt name originalName -> do
      return $ CC.Typed qt area (CC.Extern qt name originalName)

    Core.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- convert env exp
      placeholderRef' <- convertPlaceholderRef placeholderRef
      return $ CC.Typed qt area (CC.Placeholder (placeholderRef', ts) exp')



convertPlaceholderRef :: Core.PlaceholderRef -> Convert CC.PlaceholderRef
convertPlaceholderRef phr = case phr of
  Core.ClassRef cls ps call var -> do
    ps'  <- mapM convertClassRefPred ps
    return $ CC.ClassRef cls ps' call var

  Core.MethodRef cls mtd call -> do
    return $ CC.MethodRef cls mtd call


convertClassRefPred :: Core.ClassRefPred -> Convert CC.ClassRefPred
convertClassRefPred (Core.CRPNode cls ts var ps) = do
  ps'  <- mapM convertClassRefPred ps
  return $ CC.CRPNode cls ts var ps'


instance Convertable Core.Typing CC.Typing where
  convert env (Core.Untyped area typing) = case typing of
    Core.TRSingle name       -> return $ CC.Untyped area $ CC.TRSingle name

    Core.TRComp name typings -> do
      typings' <- mapM (convert env) typings
      return $ CC.Untyped area $ CC.TRComp name typings'

    Core.TRArr left right -> do
      left'  <- convert env left
      right' <- convert env right
      return $ CC.Untyped area $ CC.TRArr left' right'

    Core.TRRecord fields base -> do
      fields' <- mapM (convert env) fields
      base'   <- mapM (convert env) base
      return $ CC.Untyped area $ CC.TRRecord fields' base'

    Core.TRTuple typings -> do
      typings' <- mapM (convert env) typings
      return $ CC.Untyped area $ CC.TRTuple typings'

    Core.TRConstrained constraints typing -> do
      constraints' <- mapM (convert env) constraints
      typing'      <- convert env typing
      return $ CC.Untyped area $ CC.TRConstrained constraints' typing'

instance Convertable Core.ListItem CC.ListItem where
  convert env (Core.Typed qt@(_ :=> t) area item) = case item of
    Core.ListItem exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.ListItem exp'

    Core.ListSpread exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.ListSpread exp'

instance Convertable Core.Field CC.Field where
  convert env (Core.Typed qt@(_ :=> t) area item) = case item of
    Core.Field (name, exp) -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.Field (name, exp')

    Core.FieldSpread exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.FieldSpread exp'

instance Convertable Core.Is CC.Is where
  convert env (Core.Typed qt@(_ :=> t) area (Core.Is pat exp)) = do
    pat' <- convert env pat
    exp' <- convert env exp
    return $ CC.Typed qt area (CC.Is pat' exp')

instance Convertable Core.Pattern CC.Pattern where
  convert env (Core.Typed qt@(_ :=> t) area pat) = case pat of
    Core.PVar name       -> return $ CC.Typed qt area $ CC.PVar name

    Core.PAny            -> return $ CC.Typed qt area CC.PAny

    Core.PCon name pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PCon name pats'

    Core.PNum    num  -> return $ CC.Typed qt area $ CC.PNum num

    Core.PStr    str  -> return $ CC.Typed qt area $ CC.PStr str

    Core.PBool   boo  -> return $ CC.Typed qt area $ CC.PBool boo

    Core.PRecord pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PRecord pats'

    Core.PList pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PList pats'

    Core.PTuple pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PTuple pats'

    Core.PSpread pat -> do
      pat' <- convert env pat
      return $ CC.Typed qt area $ CC.PSpread pat'

instance Convertable Core.TypeDecl CC.TypeDecl where
  convert env (Core.Untyped area typeDecl) = case typeDecl of
    adt@Core.ADT{} -> do
      ctors <- mapM convertConstructors $ Core.adtconstructors adt
      return $ CC.Untyped area $ CC.ADT { CC.adtname         = Core.adtname adt
                                          , CC.adtparams       = Core.adtparams adt
                                          , CC.adtconstructors = ctors
                                          , CC.adtexported     = Core.adtexported adt
                                          }

    alias@Core.Alias{} -> do
      aliastype <- convert env $ Core.aliastype alias
      return $ CC.Untyped area $ CC.Alias { CC.aliasname     = Core.aliasname alias
                                            , CC.aliasparams   = Core.aliasparams alias
                                            , CC.aliastype     = aliastype
                                            , CC.aliasexported = Core.aliasexported alias
                                            }
   where
    convertConstructors :: Core.Constructor -> Convert CC.Constructor
    convertConstructors (Core.Untyped a (Core.Constructor name typings t)) = do
      typings' <- mapM (convert env) typings
      return $ CC.Untyped area $ CC.Constructor name typings' t


instance Convertable Core.Interface CC.Interface where
  convert env (Core.Untyped area (Core.Interface name constraints vars methods methodTypings)) = do
    methodTypings' <- mapM (convert env) methodTypings
    return $ CC.Untyped area $ CC.Interface name constraints vars methods methodTypings'

instance Convertable Core.Instance CC.Instance where
  convert env (Core.Untyped area (Core.Instance interface constraints pred methods)) = do
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> convert env exp) methods
    return $ CC.Untyped area $ CC.Instance interface constraints pred methods'

instance Convertable Core.Import CC.Import where
  convert _ (Core.Untyped area imp) = case imp of
    Core.NamedImport names relPath absPath ->
      return $ CC.Untyped area $ CC.NamedImport (convertImportName <$> names) relPath absPath

    Core.DefaultImport namespace relPath absPath ->
      return $ CC.Untyped area $ CC.DefaultImport (convertImportName namespace) relPath absPath


convertImportName :: Core.Core Core.Name -> CC.ClosureConverted CC.Name
convertImportName (Core.Untyped area name) = CC.Untyped area name

getMethodNames :: Core.Interface -> [String]
getMethodNames interface = case interface of
  Core.Untyped _ (Core.Interface _ _ _ methods _) ->
    M.keys methods

getConstructorNames :: [Core.TypeDecl] -> [String]
getConstructorNames typeDeclarations = case typeDeclarations of
  (td : tds) -> case td of
    Core.Untyped _ Core.ADT{ Core.adtconstructors } ->
      let constructorNames = (\(Core.Untyped _ (Core.Constructor name _ _)) -> name) <$> adtconstructors
          nextNames = getConstructorNames tds
      in  constructorNames ++ nextNames

    _ ->
      getConstructorNames tds

  [] ->
    []


getGlobalsFromImports :: [Core.Import] -> [String]
getGlobalsFromImports imports = case imports of
  (imp : nextImports) -> case imp of
    Core.Untyped _ (Core.NamedImport names _ _) ->
      (Core.getValue <$> names) ++ getGlobalsFromImports nextImports

    _ ->
      getGlobalsFromImports nextImports

  [] ->
    []


instance Convertable Core.AST CC.AST where
  convert env ast = do
    let globalVars         = mapMaybe Core.getExpName $ Core.aexps ast
        globalMethods      = concatMap getMethodNames $ Core.ainterfaces ast
        globalConstructors = getConstructorNames $ Core.atypedecls ast
        globalsFromImports = getGlobalsFromImports $ Core.aimports ast
        -- TODO: also generate freevars for imports and rename freeVars env in globalVars
        env' = env { freeVars = globalVars ++ globalMethods ++ globalConstructors ++ globalsFromImports ++ ["$"] }

    imports    <- mapM (convert env') $ Core.aimports ast
    exps       <- mapM (convert env') $ Core.aexps ast
    typeDecls  <- mapM (convert env') $ Core.atypedecls ast
    interfaces <- mapM (convert env') $ Core.ainterfaces ast
    instances  <- mapM (convert env') $ Core.ainstances ast

    defs <- getTopLevelExps
    resetTopLevelExps

    return $ CC.AST { CC.aimports    = imports
                     , CC.aexps       = defs ++ exps
                     , CC.atypedecls  = typeDecls
                     , CC.ainterfaces = interfaces
                     , CC.ainstances  = instances
                     , CC.apath       = Core.apath ast
                     }


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
convertTable :: Core.Table -> CC.Table
convertTable table =
  let env       = Env { freeVars = [], freeVarExclusion = [], stillTopLevel = True, lifted = M.empty }
      convertd = mapM (convert env) table
  in  MonadState.evalState convertd initialOptimizationState
