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
import qualified AST.PostProcessed             as PP
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


findFreeVars :: Env -> PP.Exp -> Convert [(String, CC.Exp)]
findFreeVars env exp = do
  fvs <- case exp of
    PP.Typed _ _ (PP.Var "+") ->
      return []

    PP.Typed _ _ (PP.Var "++") ->
      return []

    PP.Typed _ _ (PP.Var "-") ->
      return []

    PP.Typed _ _ (PP.Var "*") ->
      return []

    PP.Typed _ _ (PP.Var "/") ->
      return []

    PP.Typed _ _ (PP.Var "==") ->
      return []

    PP.Typed _ _ (PP.Var "!=") ->
      return []

    PP.Typed _ _ (PP.Var "!") ->
      return []

    PP.Typed _ _ (PP.Var ">") ->
      return []

    PP.Typed _ _ (PP.Var "<") ->
      return []

    PP.Typed _ _ (PP.Var ">=") ->
      return []

    PP.Typed _ _ (PP.Var "<=") ->
      return []

    PP.Typed _ _ (PP.Var "&&") ->
      return []

    PP.Typed _ _ (PP.Var "%") ->
      return []

    -- field access should not be registered as a free var
    PP.Typed _ _ (PP.Var ('.' : _)) ->
      return []

    PP.Typed _ _ (PP.Var n) -> do
      var' <- convert env exp
      return [(n, var')]

    PP.Typed _ _ (PP.Definition _ params body) -> do
      vars <- findFreeVarsInBody env body
      return $ filter (\(varName, _) -> varName `notElem` params) vars
      where
        findFreeVarsInBody :: Env -> [PP.Exp] -> Convert [(String, CC.Exp)]
        findFreeVarsInBody env exps = case exps of
          (e : es) -> case e of
            PP.Typed _ _ (PP.Assignment name exp) -> do
              fvs     <- findFreeVars (addGlobalFreeVar name env) exp
              nextFVs <- findFreeVarsInBody (addGlobalFreeVar name env) es
              return $ fvs ++ nextFVs

            PP.Typed _ _ (PP.TypedExp (PP.Typed _ _ (PP.Assignment name exp)) _) -> do
              fvs     <- findFreeVars (addGlobalFreeVar name env) exp
              nextFVs <- findFreeVarsInBody (addGlobalFreeVar name env) es
              return $ fvs ++ nextFVs

            _ -> do
              fvs     <- findFreeVars env e
              nextFVs <- findFreeVarsInBody env es
              return $ fvs ++ nextFVs

          [] ->
            return []

    PP.Typed _ _ (PP.Call _ f args) -> do
      fFreeVars   <- findFreeVars env f
      argFreeVars <- concat <$> mapM (findFreeVars env) args
      return $ fFreeVars ++ argFreeVars

    PP.Typed _ _ (PP.Do exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    PP.Typed _ _ (PP.If cond truthy falsy) -> do
      condFreeVars   <- findFreeVars env cond
      truthyFreeVars <- findFreeVars env truthy
      falsyFreeVars  <- findFreeVars env falsy
      return $ condFreeVars ++ truthyFreeVars ++ falsyFreeVars

    PP.Typed _ _ (PP.TupleConstructor exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    PP.Typed _ _ (PP.TemplateString exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    PP.Typed _ _ (PP.Access record field) -> do
      recordVars <- findFreeVars env record
      fieldVars  <- findFreeVars env field
      return $ recordVars ++ fieldVars

    PP.Typed _ _ (PP.ListConstructor exps) -> do
      vars <- mapM (findFreeVars env . PP.getListItemExp) exps
      return $ concat vars

    PP.Typed _ _ (PP.Where whereExp iss) -> do
      expVars     <- findFreeVars env whereExp
      issFreeVars <- findFreeVarsInBranches env iss
      return $ expVars ++ issFreeVars

    PP.Typed _ _ (PP.Assignment n exp) -> do
      findFreeVars env exp

    PP.Typed _ _ (PP.TypedExp exp _) -> do
      findFreeVars env exp

    -- TODO: Check that we still need this
    PP.Typed (_ :=> t) area (PP.LNum x) -> case t of
      TVar _ -> do
        let dictName = "$Number$" <> Types.buildTypeStrForPlaceholder [t]
        if dictName `notElem` freeVars env then
          return [(dictName, CC.Typed ([] :=> tVar "dict") area (CC.Var dictName))]
        else
          return []

      _ ->
        return []

    PP.Typed _ area (PP.Placeholder ph exp) -> do
      (placeholderVars, excludeVars) <- case ph of
        (PP.ClassRef interface _ _ True, ts) ->
          let dictName = "$" <> interface <> "$" <> ts
          in  return ([(dictName, CC.Typed ([] :=> tVar "dict") area (CC.Var dictName))], [])

        (PP.MethodRef interface methodName True, ts) -> do
          let dictName = "$" <> interface <> "$" <> ts
          return ([(dictName, CC.Typed ([] :=> tVar "dict") area (CC.Var dictName))], [methodName])

        _ ->
          return ([], [])
      expVars <- case ph of
        -- If it's a resolved method, it is accessed from the global scope
        (PP.MethodRef _ _ False, _) ->
          return []

        _ ->
          findFreeVars env exp
      return $ filter (\(varName, _) -> varName `notElem` excludeVars) $ placeholderVars ++ expVars

    PP.Typed _ _ (PP.Record fields) -> do
      fvs <- mapM findFreeVarsInField fields
      return $ concat fvs
      where
        findFreeVarsInField :: PP.Field -> Convert [(String, CC.Exp)]
        findFreeVarsInField field = case field of
          PP.Typed _ _ (PP.Field (_, exp)) ->
            findFreeVars env exp

          PP.Typed _ _ (PP.FieldSpread exp) ->
            findFreeVars env exp

    _ ->
      return []

  let globalVars = freeVars env ++ M.keys (lifted env)
  let fvs' = M.toList $ M.fromList fvs

  return $ filter (\(varName, _) -> varName `notElem` globalVars || varName `elem` freeVarExclusion env) fvs'

findFreeVarsInBranches :: Env -> [PP.Is] -> Convert [(String, CC.Exp)]
findFreeVarsInBranches env iss = case iss of
  (is : next) -> do
    branchVars <- findFreeVarsInBranch env is
    nextVars   <- findFreeVarsInBranches env next
    return $ branchVars ++ nextVars

  [] ->
    return []

findFreeVarsInBranch :: Env -> PP.Is -> Convert [(String, CC.Exp)]
findFreeVarsInBranch env is = case is of
  PP.Typed _ _ (PP.Is pat exp) -> do
    let patternVars = getPatternVars pat
    expVars <- findFreeVars env exp
    return $ filter (\(varName, _) -> varName `notElem` patternVars) expVars


getPatternVars :: PP.Pattern -> [String]
getPatternVars (PP.Typed _ _ pat) = case pat of
  PP.PVar n ->
    [n]

  PP.PCon _ pats ->
    concatMap getPatternVars pats

  PP.PRecord fields ->
    concatMap getPatternVars $ M.elems fields

  PP.PList pats ->
    concatMap getPatternVars pats

  PP.PTuple pats ->
    concatMap getPatternVars pats

  PP.PSpread pat' ->
    getPatternVars pat'

  _ ->
    []


class Convertable a b where
  convert :: Env -> a -> Convert b


-- At this point it's no longer top level and all functions encountered must be lifted
convertBody :: [String] -> Env -> [PP.Exp] -> Convert [CC.Exp]
convertBody exclusionVars env body = case body of
  [] ->
    return []

  (exp : es) -> case exp of
    PP.Typed _ _ (PP.TypedExp (PP.Typed qt@(_ :=> t) area (PP.Assignment name abs@(PP.Typed _ _ (PP.Definition _ params body)))) _) -> do
      exp' <- convertAbs (addVarExclusions exclusionVars env) name [] abs
      next <- convertBody (name : exclusionVars) (addGlobalFreeVar name env) es
      return $ exp' : next

    PP.Typed qt@(_ :=> t) area (PP.Assignment name abs@(PP.Typed _ _ (PP.Definition _ params body))) -> do
      exp' <- convertAbs (addVarExclusions exclusionVars env) name [] abs
      next <- convertBody (name : exclusionVars) (addGlobalFreeVar name env) es
      return $ exp' : next

    abs@(PP.Typed _ _ (PP.Definition _ params body)) -> do
      exp' <- convert (addVarExclusions exclusionVars env) abs
      next <- convertBody exclusionVars env es
      return $ exp' : next

    PP.Typed _ _ (PP.TypedExp (PP.Typed _ _ (PP.Assignment name e)) _) -> do
      e'   <- convert env exp
      next <- convertBody (name : exclusionVars) env es
      return $ e' : next

    PP.Typed _ _ (PP.Assignment name e) -> do
      e'   <- convert env exp
      next <- convertBody (name : exclusionVars) env es
      return $ e' : next

    _ -> do
      exp' <- convert env exp
      next <- convertBody exclusionVars env es
      return $ exp' : next


collectPlaceholderParams :: PP.Exp -> ([String], PP.Exp)
collectPlaceholderParams ph = case ph of
  PP.Typed _ _ (PP.Placeholder (PP.ClassRef interfaceName _ False True, ts) next) ->
    let (nextParams, nextBody) = collectPlaceholderParams next
    in  ("$" <> interfaceName <> "$" <> ts : nextParams, nextBody)

  or ->
    ([], or)


convertDefType :: PP.DefinitionType -> CC.DefinitionType
convertDefType defType = case defType of
  PP.BasicDefinition ->
    CC.BasicDefinition

  PP.TCEOptimizableDefinition ->
    CC.TCEOptimizableDefinition

convertCallType :: PP.CallType -> CC.CallType
convertCallType defType = case defType of
  PP.SimpleCall ->
    CC.SimpleCall

  PP.RecursiveTailCall ->
    CC.RecursiveTailCall


convertAbs :: Env -> String -> [String] -> PP.Exp -> Convert CC.Exp
convertAbs env functionName placeholders abs@(PP.Typed (ps :=> t) area (PP.Definition defType params body)) = do
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


instance Convertable PP.Exp CC.Exp where
  convert _ (PP.Untyped area (PP.TypeExport name)) = return $ CC.Untyped area (CC.TypeExport name)
  convert env fullExp@(PP.Typed qt@(ps :=> t) area e) = case e of
    PP.LNum x -> case t of
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

    PP.LFloat x ->
      return $ CC.Typed qt area (CC.LFloat x)

    PP.LStr x ->
      return $ CC.Typed qt area (CC.LStr x)

    PP.LBool x ->
      return $ CC.Typed qt area (CC.LBool x)

    PP.LUnit ->
      return $ CC.Typed qt area CC.LUnit

    PP.TemplateString es -> do
      es' <- mapM (convert env { stillTopLevel = False }) es
      return $ CC.Typed qt area (CC.TemplateString es')

    PP.JSExp js         -> return $ CC.Typed qt area (CC.JSExp js)

    PP.Call callType fn args -> do
        fn'   <- convert env { stillTopLevel = False } fn
        args' <- mapM (convert env { stillTopLevel = False }) args
        return $ CC.Typed qt area (CC.Call (convertCallType callType) fn' args')

    PP.Access rec field -> do
      rec'   <- convert env { stillTopLevel = False } rec
      field' <- convert env { stillTopLevel = False } field
      return $ CC.Typed qt area (CC.Access rec' field')

    PP.Export (PP.Typed _ _ (PP.Assignment name abs@(PP.Typed _ _ (PP.Definition _ params body)))) -> do
      convertAbs env name [] abs

    PP.TypedExp (PP.Typed _ _ (PP.Export (PP.Typed _ _ (PP.Assignment name abs@(PP.Typed _ _ (PP.Definition _ params body)))))) _ -> do
      convertAbs env name [] abs

    PP.TypedExp (PP.Typed _ _ (PP.Assignment name abs@(PP.Typed _ _ (PP.Definition _ params body)))) _ -> do
      convertAbs env name [] abs

    PP.Assignment name abs@(PP.Typed _ _ (PP.Definition _ params body)) -> do
      convertAbs env name [] abs

    -- unnamed abs, we need to generate a name here
    PP.Definition defType params body -> do
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

    PP.Assignment functionName ph@(PP.Typed _ _ (PP.Placeholder (placeholderRef@(PP.ClassRef interfaceName _ False _), ts) exp)) -> do
      let isTopLevel = stillTopLevel env
      if isTopLevel then do
        let (params, innerExp)   = collectPlaceholderParams ph
        let typeWithPlaceholders = foldr fn t (tVar "dict" <$ params)
        case innerExp of
          PP.Typed _ _ (PP.Definition _ _ _) -> do
            convertAbs env functionName params innerExp
          _ -> do
            innerExp' <- convert env { stillTopLevel = False } innerExp
            return $ CC.Typed (ps :=> typeWithPlaceholders) area $ CC.Definition CC.BasicDefinition functionName params [innerExp']
      else do
        let (dictParams, innerExp) = collectPlaceholderParams ph
            isFunction = isFunctionType (PP.getType exp)
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
    PP.Assignment name exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area (CC.Assignment name exp' (stillTopLevel env))

    PP.Export exp -> do
      convert env exp

    PP.NameExport name     ->
      return $ CC.Typed qt area (CC.NameExport name)

    PP.Var        name     -> case M.lookup name (lifted env) of
      Just (newName, capturedArgs) ->
        return $ CC.Typed qt area (CC.Call CC.SimpleCall (CC.Typed qt area (CC.Var newName)) capturedArgs)

      Nothing ->
        return $ CC.Typed qt area (CC.Var name)

    PP.TypedExp exp scheme -> do
      exp' <- convert env exp
      return $ CC.Typed qt area (CC.TypedExp exp' scheme)

    PP.ListConstructor items -> do
      items' <- mapM (convert env) items
      return $ CC.Typed qt area (CC.ListConstructor items')

    PP.TupleConstructor exps -> do
      exps' <- mapM (convert env) exps
      return $ CC.Typed qt area (CC.TupleConstructor exps')

    PP.Record fields -> do
      fields' <- mapM (convert env { stillTopLevel = False }) fields
      return $ CC.Typed qt area (CC.Record fields')

    PP.If cond truthy falsy -> do
      cond'   <- convert env { stillTopLevel = False } cond
      truthy' <- convert env { stillTopLevel = False } truthy
      falsy'  <- convert env { stillTopLevel = False } falsy
      return $ CC.Typed qt area (CC.If cond' truthy' falsy')

    PP.Do exps -> do
      exps' <- convertBody [] env { stillTopLevel = False } exps
      return $ CC.Typed qt area (CC.Do exps')

    PP.Where exp iss -> do
      exp' <- convert env { stillTopLevel = False } exp
      iss' <- mapM (convert env { stillTopLevel = False }) iss
      return $ CC.Typed qt area (CC.Where exp' iss')

    PP.Extern qt name originalName -> do
      return $ CC.Typed qt area (CC.Extern qt name originalName)

    PP.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- convert env exp
      placeholderRef' <- convertPlaceholderRef placeholderRef
      return $ CC.Typed qt area (CC.Placeholder (placeholderRef', ts) exp')



convertPlaceholderRef :: PP.PlaceholderRef -> Convert CC.PlaceholderRef
convertPlaceholderRef phr = case phr of
  PP.ClassRef cls ps call var -> do
    ps'  <- mapM convertClassRefPred ps
    return $ CC.ClassRef cls ps' call var

  PP.MethodRef cls mtd call -> do
    return $ CC.MethodRef cls mtd call


convertClassRefPred :: PP.ClassRefPred -> Convert CC.ClassRefPred
convertClassRefPred (PP.CRPNode cls ts var ps) = do
  ps'  <- mapM convertClassRefPred ps
  return $ CC.CRPNode cls ts var ps'


instance Convertable PP.Typing CC.Typing where
  convert env (PP.Untyped area typing) = case typing of
    PP.TRSingle name       -> return $ CC.Untyped area $ CC.TRSingle name

    PP.TRComp name typings -> do
      typings' <- mapM (convert env) typings
      return $ CC.Untyped area $ CC.TRComp name typings'

    PP.TRArr left right -> do
      left'  <- convert env left
      right' <- convert env right
      return $ CC.Untyped area $ CC.TRArr left' right'

    PP.TRRecord fields base -> do
      fields' <- mapM (convert env) fields
      base'   <- mapM (convert env) base
      return $ CC.Untyped area $ CC.TRRecord fields' base'

    PP.TRTuple typings -> do
      typings' <- mapM (convert env) typings
      return $ CC.Untyped area $ CC.TRTuple typings'

    PP.TRConstrained constraints typing -> do
      constraints' <- mapM (convert env) constraints
      typing'      <- convert env typing
      return $ CC.Untyped area $ CC.TRConstrained constraints' typing'

instance Convertable PP.ListItem CC.ListItem where
  convert env (PP.Typed qt@(_ :=> t) area item) = case item of
    PP.ListItem exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.ListItem exp'

    PP.ListSpread exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.ListSpread exp'

instance Convertable PP.Field CC.Field where
  convert env (PP.Typed qt@(_ :=> t) area item) = case item of
    PP.Field (name, exp) -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.Field (name, exp')

    PP.FieldSpread exp -> do
      exp' <- convert env exp
      return $ CC.Typed qt area $ CC.FieldSpread exp'

instance Convertable PP.Is CC.Is where
  convert env (PP.Typed qt@(_ :=> t) area (PP.Is pat exp)) = do
    pat' <- convert env pat
    exp' <- convert env exp
    return $ CC.Typed qt area (CC.Is pat' exp')

instance Convertable PP.Pattern CC.Pattern where
  convert env (PP.Typed qt@(_ :=> t) area pat) = case pat of
    PP.PVar name       -> return $ CC.Typed qt area $ CC.PVar name

    PP.PAny            -> return $ CC.Typed qt area CC.PAny

    PP.PCon name pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PCon name pats'

    PP.PNum    num  -> return $ CC.Typed qt area $ CC.PNum num

    PP.PStr    str  -> return $ CC.Typed qt area $ CC.PStr str

    PP.PBool   boo  -> return $ CC.Typed qt area $ CC.PBool boo

    PP.PRecord pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PRecord pats'

    PP.PList pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PList pats'

    PP.PTuple pats -> do
      pats' <- mapM (convert env) pats
      return $ CC.Typed qt area $ CC.PTuple pats'

    PP.PSpread pat -> do
      pat' <- convert env pat
      return $ CC.Typed qt area $ CC.PSpread pat'

instance Convertable PP.TypeDecl CC.TypeDecl where
  convert env (PP.Untyped area typeDecl) = case typeDecl of
    adt@PP.ADT{} -> do
      ctors <- mapM convertConstructors $ PP.adtconstructors adt
      return $ CC.Untyped area $ CC.ADT { CC.adtname         = PP.adtname adt
                                          , CC.adtparams       = PP.adtparams adt
                                          , CC.adtconstructors = ctors
                                          , CC.adtexported     = PP.adtexported adt
                                          }

    alias@PP.Alias{} -> do
      aliastype <- convert env $ PP.aliastype alias
      return $ CC.Untyped area $ CC.Alias { CC.aliasname     = PP.aliasname alias
                                            , CC.aliasparams   = PP.aliasparams alias
                                            , CC.aliastype     = aliastype
                                            , CC.aliasexported = PP.aliasexported alias
                                            }
   where
    convertConstructors :: PP.Constructor -> Convert CC.Constructor
    convertConstructors (PP.Untyped a (PP.Constructor name typings t)) = do
      typings' <- mapM (convert env) typings
      return $ CC.Untyped area $ CC.Constructor name typings' t


instance Convertable PP.Interface CC.Interface where
  convert env (PP.Untyped area (PP.Interface name constraints vars methods methodTypings)) = do
    methodTypings' <- mapM (convert env) methodTypings
    return $ CC.Untyped area $ CC.Interface name constraints vars methods methodTypings'

instance Convertable PP.Instance CC.Instance where
  convert env (PP.Untyped area (PP.Instance interface constraints pred methods)) = do
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> convert env exp) methods
    return $ CC.Untyped area $ CC.Instance interface constraints pred methods'

instance Convertable PP.Import CC.Import where
  convert _ (PP.Untyped area imp) = case imp of
    PP.NamedImport names relPath absPath ->
      return $ CC.Untyped area $ CC.NamedImport (convertImportName <$> names) relPath absPath

    PP.DefaultImport namespace relPath absPath ->
      return $ CC.Untyped area $ CC.DefaultImport (convertImportName namespace) relPath absPath


convertImportName :: PP.PostProcessed PP.Name -> CC.ClosureConverted CC.Name
convertImportName (PP.Untyped area name) = CC.Untyped area name

getMethodNames :: PP.Interface -> [String]
getMethodNames interface = case interface of
  PP.Untyped _ (PP.Interface _ _ _ methods _) ->
    M.keys methods

getConstructorNames :: [PP.TypeDecl] -> [String]
getConstructorNames typeDeclarations = case typeDeclarations of
  (td : tds) -> case td of
    PP.Untyped _ PP.ADT{ PP.adtconstructors } ->
      let constructorNames = (\(PP.Untyped _ (PP.Constructor name _ _)) -> name) <$> adtconstructors
          nextNames = getConstructorNames tds
      in  constructorNames ++ nextNames

    _ ->
      getConstructorNames tds

  [] ->
    []


getGlobalsFromImports :: [PP.Import] -> [String]
getGlobalsFromImports imports = case imports of
  (imp : nextImports) -> case imp of
    PP.Untyped _ (PP.NamedImport names _ _) ->
      (PP.getValue <$> names) ++ getGlobalsFromImports nextImports

    _ ->
      getGlobalsFromImports nextImports

  [] ->
    []


instance Convertable PP.AST CC.AST where
  convert env ast = do
    let globalVars         = mapMaybe PP.getExpName $ PP.aexps ast
        globalMethods      = concatMap getMethodNames $ PP.ainterfaces ast
        globalConstructors = getConstructorNames $ PP.atypedecls ast
        globalsFromImports = getGlobalsFromImports $ PP.aimports ast
        -- TODO: also generate freevars for imports and rename freeVars env in globalVars
        env' = env { freeVars = globalVars ++ globalMethods ++ globalConstructors ++ globalsFromImports ++ ["$"] }

    imports    <- mapM (convert env') $ PP.aimports ast
    exps       <- mapM (convert env') $ PP.aexps ast
    typeDecls  <- mapM (convert env') $ PP.atypedecls ast
    interfaces <- mapM (convert env') $ PP.ainterfaces ast
    instances  <- mapM (convert env') $ PP.ainstances ast

    defs <- getTopLevelExps
    resetTopLevelExps

    return $ CC.AST { CC.aimports    = imports
                     , CC.aexps       = defs ++ exps
                     , CC.atypedecls  = typeDecls
                     , CC.ainterfaces = interfaces
                     , CC.ainstances  = instances
                     , CC.apath       = PP.apath ast
                     }


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
convertTable :: PP.Table -> CC.Table
convertTable table =
  let env       = Env { freeVars = [], freeVarExclusion = [], stillTopLevel = True, lifted = M.empty }
      convertd = mapM (convert env) table
  in  MonadState.evalState convertd initialOptimizationState
