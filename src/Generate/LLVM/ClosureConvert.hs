{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.ClosureConvert where

import           Control.Monad.State
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.List
import qualified AST.Solved                    as Slv
import qualified Generate.LLVM.Optimized       as Opt
import           Infer.Type
import Data.Maybe
import Debug.Trace
import Text.Show.Pretty
import Explain.Location
import Generate.LLVM.Optimized (Exp_(TopLevelAbs))
import AST.Solved (getType)


data OptimizationState
  = OptimizationState { count :: Int, topLevel :: [Opt.Exp] }

data Env
  = Env
  { freeVars :: [Opt.Name]
  , freeVarExclusion :: [Opt.Name]
  -- ^ Closured names that are reassigned. So if we find something in the higher scope, the inner function should not skip it but create a param for it
  , stillTopLevel :: Bool
  , lifted :: M.Map String (String, [Opt.Exp])
  -- ^ the key is the initial name, and then we have (lifted name, args to partially apply)
  }

initialOptimizationState :: OptimizationState
initialOptimizationState = OptimizationState { count = 0, topLevel = [] }

type Optimize a = forall m . MonadState OptimizationState m => m a


numbers :: [String]
numbers = show <$> [0 ..]

generateLiftedName :: String -> Optimize String
generateLiftedName originalName = do
  s@(OptimizationState count _) <- get
  let index = numbers !! count
  let name = originalName ++ "$lifted$" ++ index
  put s { count = count + 1 }
  return name

resetTopLevelExps :: Optimize ()
resetTopLevelExps = do
  s@(OptimizationState _ topLevel) <- get
  put s { topLevel = [] }

addTopLevelExp :: Opt.Exp -> Optimize ()
addTopLevelExp exp = do
  s@(OptimizationState _ topLevel) <- get
  put s { topLevel = topLevel ++ [exp] }

getTopLevelExps :: Optimize [Opt.Exp]
getTopLevelExps = do
  OptimizationState _ topLevel <- get
  return topLevel

addGlobalFreeVar :: Opt.Name -> Env -> Env
addGlobalFreeVar fv env =
  env { freeVars = fv : freeVars env }

addVarExclusion :: String -> Env -> Env
addVarExclusion var env =
  env { freeVarExclusion = var : freeVarExclusion env }

addVarExclusions :: [String] -> Env -> Env
addVarExclusions vars env =
  env { freeVarExclusion = vars ++ freeVarExclusion env }

addLiftedLambda :: Opt.Name -> Opt.Name -> [Opt.Exp] -> Env -> Env
addLiftedLambda originalName liftedName args env =
  env { lifted = M.insert originalName (liftedName, args) $ lifted env }


findFreeVars :: Env -> Slv.Exp -> Optimize [(String, Opt.Exp)]
findFreeVars env exp = do
  fvs <- case exp of
    Slv.Solved _ _ (Slv.Var "+") ->
      return []

    Slv.Solved _ _ (Slv.Var "++") ->
      return []

    Slv.Solved _ _ (Slv.Var "-") ->
      return []

    Slv.Solved _ _ (Slv.Var "*") ->
      return []

    Slv.Solved _ _ (Slv.Var "/") ->
      return []

    Slv.Solved _ _ (Slv.Var "==") ->
      return []

    Slv.Solved _ _ (Slv.Var "!=") ->
      return []

    Slv.Solved _ _ (Slv.Var "!") ->
      return []

    Slv.Solved _ _ (Slv.Var ">") ->
      return []

    Slv.Solved _ _ (Slv.Var "<") ->
      return []

    Slv.Solved _ _ (Slv.Var ">=") ->
      return []

    Slv.Solved _ _ (Slv.Var "<=") ->
      return []

    Slv.Solved _ _ (Slv.Var "&&") ->
      return []

    -- field access should not be registered as a free var
    Slv.Solved _ _ (Slv.Var ('.' : _)) ->
      return []

    Slv.Solved _ _ (Slv.Var n) -> do
      var' <- optimize env exp
      return [(n, var')]

    Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body) -> do
      vars <- findFreeVarsInBody env body
      return $ filter (\(varName, _) -> varName /= param) vars
      where
        findFreeVarsInBody :: Env -> [Slv.Exp] -> Optimize [(String, Opt.Exp)]
        findFreeVarsInBody env exps = case exps of
          (e : es) -> case e of
            Slv.Solved _ _ (Slv.Assignment name exp) -> do
              fvs     <- findFreeVars (addGlobalFreeVar name env) exp
              nextFVs <- findFreeVarsInBody (addGlobalFreeVar name env) es
              return $ fvs ++ nextFVs

            _ -> do
              fvs     <- findFreeVars env e
              nextFVs <- findFreeVarsInBody env es
              return $ fvs ++ nextFVs

          [] ->
            return []

    Slv.Solved _ _ (Slv.App f arg _) -> do
      fFreeVars   <- findFreeVars env f
      argFreeVars <- findFreeVars env arg
      return $ fFreeVars ++ argFreeVars

    Slv.Solved _ _ (Slv.Do exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Slv.Solved _ _ (Slv.If cond truthy falsy) -> do
      condFreeVars   <- findFreeVars env cond
      truthyFreeVars <- findFreeVars env truthy
      falsyFreeVars  <- findFreeVars env falsy
      return $ condFreeVars ++ truthyFreeVars ++ falsyFreeVars

    Slv.Solved _ _ (Slv.TupleConstructor exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Slv.Solved _ _ (Slv.TemplateString exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Slv.Solved _ _ (Slv.Access record field) -> do
      recordVars <- findFreeVars env record
      fieldVars  <- findFreeVars env field
      return $ recordVars ++ fieldVars

    Slv.Solved _ _ (Slv.ListConstructor exps) -> do
      vars <- mapM (findFreeVars env . Slv.getListItemExp) exps
      return $ concat vars

    Slv.Solved _ _ (Slv.Where whereExp iss) -> do
      expVars     <- findFreeVars env whereExp
      issFreeVars <- findFreeVarsInBranches env iss
      return $ expVars ++ issFreeVars

    Slv.Solved _ _ (Slv.Assignment n exp) -> do
      findFreeVars env exp

    Slv.Solved _ _ (Slv.TypedExp exp _ _) -> do
      findFreeVars env exp

    Slv.Solved (_ :=> t) area (Slv.LNum x) -> case t of
      TVar _ -> do
        let dictName = "$Number$" <> buildTypeStrForPlaceholder [t]
        if dictName `notElem` freeVars env then
          return [(dictName, Opt.Optimized ([] :=> tVar "dict") area (Opt.Var dictName))]
        else
          return []
      
      _ ->
        return []

    Slv.Solved _ area (Slv.Placeholder ph exp) -> do
      (placeholderVars, excludeVars) <- case ph of
        -- (Slv.ClassRef interface _ True True, ts) ->
        (Slv.ClassRef interface _ _ True, ts) ->
          let tsStr = buildTypeStrForPlaceholder ts
              dictName = "$" <> interface <> "$" <> tsStr
          in  return ([(dictName, Opt.Optimized ([] :=> tVar "dict") area (Opt.Var dictName))], [])

        (Slv.MethodRef interface methodName True, ts) -> do
          let tsStr    = buildTypeStrForPlaceholder ts
              dictName = "$" <> interface <> "$" <> tsStr
          return ([(dictName, Opt.Optimized ([] :=> tVar "dict") area (Opt.Var dictName))], [methodName])

        _ ->
          return ([], [])
      expVars <- case ph of
        -- If it's a resolved method, it is accessed from the global scope
        (Slv.MethodRef _ _ False, _) ->
          return []

        _ ->
          findFreeVars env exp
      return $ filter (\(varName, _) -> varName `notElem` excludeVars) $ placeholderVars ++ expVars

    _ ->
      return []

  let globalVars = freeVars env ++ M.keys (lifted env)
  let fvs' = M.toList $ M.fromList fvs

  return $ filter (\(varName, _) -> varName `notElem` globalVars || varName `elem` freeVarExclusion env) fvs'

findFreeVarsInBranches :: Env -> [Slv.Is] -> Optimize [(String, Opt.Exp)]
findFreeVarsInBranches env iss = case iss of
  (is : next) -> do
    branchVars <- findFreeVarsInBranch env is
    nextVars   <- findFreeVarsInBranches env next
    return $ branchVars ++ nextVars

  [] ->
    return []

findFreeVarsInBranch :: Env -> Slv.Is -> Optimize [(String, Opt.Exp)]
findFreeVarsInBranch env is = case is of
  Slv.Solved _ _ (Slv.Is pat exp) -> do
    let patternVars = getPatternVars pat
    expVars <- findFreeVars env exp
    return $ filter (\(varName, _) -> varName `notElem` patternVars) expVars


getPatternVars :: Slv.Pattern -> [String]
getPatternVars (Slv.Solved _ _ pat) = case pat of
  Slv.PVar n ->
    [n]

  Slv.PCon _ pats ->
    concatMap getPatternVars pats

  Slv.PRecord fields ->
    concatMap getPatternVars $ M.elems fields

  Slv.PList pats ->
    concatMap getPatternVars pats

  Slv.PTuple pats ->
    concatMap getPatternVars pats

  Slv.PSpread pat' ->
    getPatternVars pat'

  _ ->
    []



class Optimizable a b where
  optimize :: Env -> a -> Optimize b




-- At this point it's no longer top level and all functions encountered must be lifted
optimizeBody :: [String] -> Env -> [Slv.Exp] -> Optimize [Opt.Exp]
optimizeBody exclusionVars env body = case body of
  [] ->
    return []

  (exp : es) -> case exp of
    Slv.Solved _ _ (Slv.TypedExp (Slv.Solved qt@(_ :=> t) area (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)))) _ _) -> do
      exp' <- optimizeAbs (addVarExclusions exclusionVars env) name abs
      next <- optimizeBody exclusionVars (addGlobalFreeVar name env) es
      return $ exp' : next

    Slv.Solved qt@(_ :=> t) area (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body))) -> do
      exp' <- optimizeAbs (addVarExclusions exclusionVars env) name abs
      next <- optimizeBody exclusionVars (addGlobalFreeVar name env) es
      return $ exp' : next

    abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)) -> do
      exp' <- optimize (addVarExclusions exclusionVars env) abs
      next <- optimizeBody exclusionVars env es
      return $ exp' : next

    Slv.Solved _ _ (Slv.Assignment name e) -> do
      e'   <- optimize env exp
      next <- optimizeBody (name : exclusionVars) env es
      return $ e' : next

    _ -> do
      exp' <- optimize env exp
      next <- optimizeBody exclusionVars env es
      return $ exp' : next



collectAbsParams :: Slv.Exp -> ([String], [Slv.Exp])
collectAbsParams abs = case abs of
  Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) [body]) ->
    let (nextParams, nextBody) = collectAbsParams body
    in  (param : nextParams, nextBody)

  Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body) ->
    ([param], body)

  b ->
    ([], [b])


collectPlaceholderParams :: Slv.Exp -> ([String], Slv.Exp)
collectPlaceholderParams ph = case ph of
  Slv.Solved _ _ (Slv.Placeholder (Slv.ClassRef interfaceName _ False True, ts) next) ->
    let (nextParams, nextBody) = collectPlaceholderParams next
        tsStr = buildTypeStrForPlaceholder ts
    in  ("$" <> interfaceName <> "$" <> tsStr : nextParams, nextBody)

  or ->
    ([], or)


collectAppArgs :: Bool -> Slv.Exp -> (Slv.Exp, [Slv.Exp])
collectAppArgs isFirst app = case app of
  Slv.Solved _ _ (Slv.App next arg isFinal) | not isFinal || isFirst ->
    let (nextFn, nextArgs) = collectAppArgs False next
    in  (nextFn, nextArgs <> [arg])

  b ->
    (b, [])


fillClosureParams :: Int -> Type -> Type
fillClosureParams paramCount t =
  let paramTypes = getParamTypes t
      missingParams = paramCount - length paramTypes
  in  foldr fn t $ replicate missingParams (tVar "filledParam")


optimizeAbs :: Env -> String -> Slv.Exp -> Optimize Opt.Exp
optimizeAbs env functionName abs@(Slv.Solved (ps :=> t) area _) = do
  let isTopLevel = stillTopLevel env
  if isTopLevel then do
    let (params, body') = collectAbsParams abs
    body'' <- optimizeBody [] env { stillTopLevel = False } body'

    -- Hacky for now
    let paramTypes = getParamTypes t
    -- let t' = fillClosureParams (length params) t
    let t' =
          if length paramTypes < length params then
            tVar "a" `fn` t
          else
            t

    return $ Opt.Optimized (ps :=> t') area $ TopLevelAbs functionName params body''
  else do
    -- here we need to add free var parameters, lift it, and if there is any free var, replace the abs with a
    -- PAP that applies the free vars from the current scope.
    let (params, body') = collectAbsParams abs
    fvs           <- findFreeVars (addGlobalFreeVar functionName env) abs
    functionName' <- generateLiftedName functionName
    body''        <- optimizeBody [] (addLiftedLambda functionName functionName' (snd <$> fvs) env) body'

    let paramsWithFreeVars = (fst <$> fvs) ++ params

    let liftedType = foldr fn t (Opt.getType . snd <$> fvs)
    let lifted = Opt.Optimized (ps :=> liftedType) area (Opt.TopLevelAbs functionName' paramsWithFreeVars body'')
    addTopLevelExp lifted

    let functionNode = Opt.Optimized (ps :=> t) area (Opt.Var functionName')

    if null fvs then
      return $ Opt.Optimized (ps :=> t) area (Opt.Assignment functionName functionNode False)
    else
      -- We need to carry types here
      let fvVarNodes = snd <$> fvs
      in  return $ Opt.Optimized (ps :=> t) area (Opt.Assignment functionName (Opt.Optimized (ps :=> t) area (Opt.App functionNode fvVarNodes)) False)



buildAbs :: [(String, Qual Type)] -> [Slv.Exp] -> Slv.Exp
buildAbs [(param, ps :=> t)] body =
  let bodyType = Slv.getType (last body)
  in  Slv.Solved (ps :=> (t `fn` bodyType)) emptyArea (Slv.Abs (Slv.Solved (ps :=> t) emptyArea param) body)
buildAbs ((param, ps :=> t) : xs) body =
  let next     = buildAbs xs body
      nextType = Slv.getType next
  in  Slv.Solved ([] :=> (t `fn` nextType)) emptyArea (Slv.Abs (Slv.Solved (ps :=> t) emptyArea param) [next])


buildApp :: Slv.Exp -> [Slv.Exp] -> Slv.Exp
buildApp f args =
  buildApp' (length args) (length args) f args

buildApp' :: Int -> Int -> Slv.Exp -> [Slv.Exp] -> Slv.Exp
buildApp' total nth f@(Slv.Solved (ps :=> t) area f') [arg] =
  Slv.Solved (ps :=> dropFirstParamType t) area (Slv.App f arg (total == nth))
buildApp' total nth f@(Slv.Solved (ps :=> t) _ f') xs =
  let arg@(Slv.Solved _ area _) = last xs
      subApp                    = buildApp' total (nth - 1) f (init xs)
  in  Slv.Solved (ps :=> dropNFirstParamTypes nth t) area (Slv.App subApp arg (total == nth))


-- Checks for Var "$" placeholders
-- Returns all the args to be applied, as well as the name of params to wrap it
placeholderArgCheck :: Int -> [Slv.Exp] -> ([Slv.Exp], [(String, Qual Type)])
placeholderArgCheck phIndex args = case args of
  (arg : next) -> case arg of
    Slv.Solved t area (Slv.Var "$") ->
      let paramName          = "__$" ++ show phIndex ++ "__"
          (nextArgs, params) = placeholderArgCheck (phIndex + 1) next
      in  (Slv.Solved t area (Slv.Var paramName) : nextArgs, (paramName, t) : params)

    _ ->
      let (nextArgs, params) = placeholderArgCheck phIndex next
      in  (arg : nextArgs, params)

  [] ->
    ([], [])


instance Optimizable Slv.Exp Opt.Exp where
  optimize _ (Slv.Untyped area (Slv.TypeExport name)) = return $ Opt.Untyped area (Opt.TypeExport name)
  optimize env fullExp@(Slv.Solved qt@(ps :=> t) area e) = case e of
    Slv.LNum x -> case t of
      TVar _ ->
        return $ Opt.Optimized qt area (
          Opt.App
            ( Opt.Optimized qt area (
                Opt.Placeholder
                  (Opt.MethodRef "Number" "__coerceNumber__" True, buildTypeStrForPlaceholder [t])
                  (Opt.Optimized qt area (Opt.Var "__coerceNumber__"))
              )
            )
            [Opt.Optimized qt area (Opt.LNum x)]
        )

      _ ->
        return $ Opt.Optimized qt area (Opt.LNum x)

    Slv.LFloat x ->
      return $ Opt.Optimized qt area (Opt.LFloat x)

    Slv.LStr x ->
      return $ Opt.Optimized qt area (Opt.LStr x)

    Slv.LBool x ->
      return $ Opt.Optimized qt area (Opt.LBool x)

    Slv.LUnit ->
      return $ Opt.Optimized qt area Opt.LUnit

    Slv.TemplateString es -> do
      es' <- mapM (optimize env { stillTopLevel = False }) es
      return $ Opt.Optimized qt area (Opt.TemplateString es')

    Slv.JSExp js         -> return $ Opt.Optimized qt area (Opt.JSExp js)

    -- TODO: handle application placeholders $
    -- to do this we need to check the collected args, if some of them are Var "$"
    -- we wrap it into an Slv.Abs and then call optimize on it.
    Slv.App fn arg close -> do
      let (fn', args)                       = collectAppArgs True fullExp
          (args', wrapperPlaceholderParams) = placeholderArgCheck 0 args
      if null wrapperPlaceholderParams then do
        let (fn'', extraArgs) = case fn' of
              Slv.Solved t area (Slv.Var fnName) -> case M.lookup fnName (lifted env) of
                Just (newName, extraArgs) ->
                  (Slv.Solved t area (Slv.Var newName), extraArgs)

                Nothing ->
                  (fn', [])

              _ ->
                (fn', [])
        fn'''  <- optimize env { stillTopLevel = False } fn''
        args' <- mapM (optimize env { stillTopLevel = False }) args
        return $ Opt.Optimized qt area (Opt.App fn''' (extraArgs ++ args'))
      else do
        -- if we found some Var "$" args we need to wrap it in an Abs
        -- params
        let appWithRenamedArgs = buildApp fn' args'
            wrapperAbs         = buildAbs wrapperPlaceholderParams [appWithRenamedArgs]
        optimize env wrapperAbs

    Slv.Access rec field -> do
      rec'   <- optimize env { stillTopLevel = False } rec
      field' <- optimize env { stillTopLevel = False } field
      return $ Opt.Optimized qt area (Opt.Access rec' field')

    Slv.Export (Slv.Solved _ _ (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)))) -> do
      optimizeAbs env name abs

    Slv.TypedExp (Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)))))) _ _ -> do
      optimizeAbs env name abs

    Slv.TypedExp (Slv.Solved _ _ (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)))) _ _ -> do
      optimizeAbs env name abs

    Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)) -> do
      optimizeAbs env name abs

    -- unnamed abs, we need to generate a name here
    Slv.Abs (Slv.Solved _ _ param) body -> do
      let (params, body') = collectAbsParams fullExp
      body''       <- optimizeBody [] env body'
      fvs          <- findFreeVars env fullExp
      functionName <- generateLiftedName "$lambda"

      let paramsWithFreeVars = (fst <$> fvs) ++ params

      let liftedType = foldr fn t (Opt.getType . snd <$> fvs)
      let lifted = Opt.Optimized (ps :=> liftedType) area (Opt.TopLevelAbs functionName paramsWithFreeVars body'')
      addTopLevelExp lifted

      let functionNode = Opt.Optimized (ps :=> liftedType) area (Opt.Var functionName)

      if null fvs then
        return functionNode
      else
        let fvVarNodes = snd <$> fvs
        in  return $ Opt.Optimized qt area (Opt.App functionNode fvVarNodes)

    Slv.Assignment functionName ph@(Slv.Solved _ _ (Slv.Placeholder (placeholderRef@(Slv.ClassRef interfaceName _ False _), ts) exp)) -> do
      let tsStr = buildTypeStrForPlaceholder ts
      let isTopLevel = stillTopLevel env
      if isTopLevel then do
        let (params, innerExp)   = collectPlaceholderParams ph
        let typeWithPlaceholders = foldr fn t (tVar "dict" <$ params)
        innerExp' <- optimize env { stillTopLevel = False } innerExp
        return $ Opt.Optimized (ps :=> typeWithPlaceholders) area $ TopLevelAbs functionName params [innerExp']
      else do
        let (dictParams, innerExp) = collectPlaceholderParams ph
            isFunction = isFunctionType (getType exp)
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
        innerExp'     <- optimize (addLiftedLambda functionName functionName' (Opt.Optimized ([] :=> tVar "dict") emptyArea . Opt.Var <$> paramsWithFreeVars) env) innerExp

        let liftedType = foldr fn t ((tVar "dict" <$ dictParams) ++ (Opt.getType . snd <$> fvs))
        let lifted = Opt.Optimized (ps :=> liftedType) area (Opt.TopLevelAbs functionName' paramsWithFreeVars [innerExp'])
        addTopLevelExp lifted

        let functionNode = Opt.Optimized (ps :=> liftedType) area (Opt.Var functionName')

        return $ Opt.Optimized qt area (Opt.Assignment functionName functionNode False)
        -- if isFunction then
        --   return $ Opt.Optimized t area (Opt.Assignment functionName functionNode False)
        -- else
        --   return $ Opt.Optimized t area (Opt.Assignment functionName (Opt.Optimized t area (Opt.App functionNode (Opt.Optimized (tVar "dict") emptyArea . Opt.Var <$> paramsWithFreeVars))) False)

    -- TODO: Add top level info so that we can generate or not the name for the global scope
    Slv.Assignment name exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized qt area (Opt.Assignment name exp' (stillTopLevel env))

    Slv.Export exp -> do
      optimize env exp
      -- exp' <- optimize env exp
      -- return $ Opt.Optimized t area (Opt.Export exp')

    Slv.NameExport name     ->
      return $ Opt.Optimized qt area (Opt.NameExport name)

    Slv.Var        name     -> case M.lookup name (lifted env) of
      Just (newName, capturedArgs) ->
        return $ Opt.Optimized qt area (Opt.App (Opt.Optimized qt area (Opt.Var newName)) capturedArgs)

      Nothing ->
        return $ Opt.Optimized qt area (Opt.Var name)

    Slv.TypedExp exp _ scheme -> do
      exp' <- optimize env exp
      return $ Opt.Optimized qt area (Opt.TypedExp exp' scheme)

    Slv.ListConstructor items -> do
      items' <- mapM (optimize env) items
      return $ Opt.Optimized qt area (Opt.ListConstructor items')

    Slv.TupleConstructor exps -> do
      exps' <- mapM (optimize env) exps
      return $ Opt.Optimized qt area (Opt.TupleConstructor exps')

    Slv.Record fields -> do
      fields' <- mapM (optimize env { stillTopLevel = False }) fields
      return $ Opt.Optimized qt area (Opt.Record fields')

    Slv.If cond truthy falsy -> do
      cond'   <- optimize env { stillTopLevel = False } cond
      truthy' <- optimize env { stillTopLevel = False } truthy
      falsy'  <- optimize env { stillTopLevel = False } falsy
      return $ Opt.Optimized qt area (Opt.If cond' truthy' falsy')

    Slv.Do exps -> do
      exps' <- optimizeBody [] env { stillTopLevel = False } exps
      return $ Opt.Optimized qt area (Opt.Do exps')

    Slv.Where exp iss -> do
      exp' <- optimize env { stillTopLevel = False } exp
      iss' <- mapM (optimize env { stillTopLevel = False }) iss
      return $ Opt.Optimized qt area (Opt.Where exp' iss')

    Slv.Extern qt name originalName -> do
      return $ Opt.Optimized qt area (Opt.Extern qt name originalName)

    Slv.Placeholder (placeholderRef, ts) exp -> case exp of
      -- Slv.Solved _ _ (Slv.Var "count") ->
      --   optimize env exp

      -- Slv.Solved _ _ (Slv.Var "initial") ->
      --   optimize env exp

      _ -> do
        exp'            <- optimize env exp
        placeholderRef' <- optimizePlaceholderRef placeholderRef
        let tsStr = buildTypeStrForPlaceholder ts
        return $ Opt.Optimized qt area (Opt.Placeholder (placeholderRef', tsStr) exp')

     where
      optimizePlaceholderRef :: Slv.PlaceholderRef -> Optimize Opt.PlaceholderRef
      optimizePlaceholderRef phr = case phr of
        Slv.ClassRef cls ps call var -> do
          ps'  <- mapM optimizeClassRefPred ps
          return $ Opt.ClassRef cls ps' call var

        Slv.MethodRef cls mtd call -> do
          return $ Opt.MethodRef cls mtd call

      optimizeClassRefPred :: Slv.ClassRefPred -> Optimize Opt.ClassRefPred
      optimizeClassRefPred (Slv.CRPNode cls ts var ps) = do
        ps'  <- mapM optimizeClassRefPred ps
        let tsStr = buildTypeStrForPlaceholder ts
        return $ Opt.CRPNode cls tsStr var ps'


instance Optimizable Slv.Typing Opt.Typing where
  optimize env (Slv.Untyped area typing) = case typing of
    Slv.TRSingle name       -> return $ Opt.Untyped area $ Opt.TRSingle name

    Slv.TRComp name typings -> do
      typings' <- mapM (optimize env) typings
      return $ Opt.Untyped area $ Opt.TRComp name typings'

    Slv.TRArr left right -> do
      left'  <- optimize env left
      right' <- optimize env right
      return $ Opt.Untyped area $ Opt.TRArr left' right'

    Slv.TRRecord fields base -> do
      fields' <- mapM (optimize env) fields
      base'   <- mapM (optimize env) base
      return $ Opt.Untyped area $ Opt.TRRecord fields' base'

    Slv.TRTuple typings -> do
      typings' <- mapM (optimize env) typings
      return $ Opt.Untyped area $ Opt.TRTuple typings'

    Slv.TRConstrained constraints typing -> do
      constraints' <- mapM (optimize env) constraints
      typing'      <- optimize env typing
      return $ Opt.Untyped area $ Opt.TRConstrained constraints' typing'

instance Optimizable Slv.ListItem Opt.ListItem where
  optimize env (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.ListItem exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized qt area $ Opt.ListItem exp'

    Slv.ListSpread exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized qt area $ Opt.ListSpread exp'

instance Optimizable Slv.Field Opt.Field where
  optimize env (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.Field (name, exp) -> do
      exp' <- optimize env exp
      return $ Opt.Optimized qt area $ Opt.Field (name, exp')

    Slv.FieldSpread exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized qt area $ Opt.FieldSpread exp'

instance Optimizable Slv.Is Opt.Is where
  optimize env (Slv.Solved qt@(_ :=> t) area (Slv.Is pat exp)) = do
    pat' <- optimize env pat
    exp' <- optimize env exp
    return $ Opt.Optimized qt area (Opt.Is pat' exp')

instance Optimizable Slv.Pattern Opt.Pattern where
  optimize env (Slv.Solved qt@(_ :=> t) area pat) = case pat of
    Slv.PVar name       -> return $ Opt.Optimized qt area $ Opt.PVar name

    Slv.PAny            -> return $ Opt.Optimized qt area Opt.PAny

    Slv.PCon name pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized qt area $ Opt.PCon name pats'

    Slv.PNum    num  -> return $ Opt.Optimized qt area $ Opt.PNum num

    Slv.PStr    str  -> return $ Opt.Optimized qt area $ Opt.PStr str

    Slv.PBool   boo  -> return $ Opt.Optimized qt area $ Opt.PBool boo

    Slv.PRecord pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized qt area $ Opt.PRecord pats'

    Slv.PList pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized qt area $ Opt.PList pats'

    Slv.PTuple pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized qt area $ Opt.PTuple pats'

    Slv.PSpread pat -> do
      pat' <- optimize env pat
      return $ Opt.Optimized qt area $ Opt.PSpread pat'

instance Optimizable Slv.TypeDecl Opt.TypeDecl where
  optimize env (Slv.Untyped area typeDecl) = case typeDecl of
    adt@Slv.ADT{} -> do
      ctors <- mapM optimizeConstructors $ Slv.adtconstructors adt
      return $ Opt.Untyped area $ Opt.ADT { Opt.adtname         = Slv.adtname adt
                                          , Opt.adtparams       = Slv.adtparams adt
                                          , Opt.adtconstructors = ctors
                                          , Opt.adtexported     = Slv.adtexported adt
                                          }

    alias@Slv.Alias{} -> do
      aliastype <- optimize env $ Slv.aliastype alias
      return $ Opt.Untyped area $ Opt.Alias { Opt.aliasname     = Slv.aliasname alias
                                            , Opt.aliasparams   = Slv.aliasparams alias
                                            , Opt.aliastype     = aliastype
                                            , Opt.aliasexported = Slv.aliasexported alias
                                            }
   where
    optimizeConstructors :: Slv.Constructor -> Optimize Opt.Constructor
    optimizeConstructors (Slv.Untyped a (Slv.Constructor name typings t)) = do
      typings' <- mapM (optimize env) typings
      return $ Opt.Untyped area $ Opt.Constructor name typings' t


instance Optimizable Slv.Interface Opt.Interface where
  optimize env (Slv.Untyped area (Slv.Interface name constraints vars methods methodTypings)) = do
    methodTypings' <- mapM (optimize env) methodTypings
    return $ Opt.Untyped area $ Opt.Interface name constraints ((\(TV n _) -> n) <$> vars) methods methodTypings'

instance Optimizable Slv.Instance Opt.Instance where
  optimize env (Slv.Untyped area (Slv.Instance interface constraints pred methods)) = do
    let typingStr = intercalate "_" (getTypeHeadName <$> predTypes pred)
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> optimize env exp) methods
    return $ Opt.Untyped area $ Opt.Instance interface constraints typingStr methods'

instance Optimizable Slv.Import Opt.Import where
  optimize _ (Slv.Untyped area imp) = case imp of
    Slv.NamedImport names relPath absPath ->
      return $ Opt.Untyped area $ Opt.NamedImport (optimizeImportName <$> names) relPath absPath

    Slv.DefaultImport namespace relPath absPath ->
      return $ Opt.Untyped area $ Opt.DefaultImport (optimizeImportName namespace) relPath absPath


optimizeImportName :: Slv.Solved Slv.Name -> Opt.Optimized Opt.Name
optimizeImportName (Slv.Untyped area name) = Opt.Untyped area name

getMethodNames :: Slv.Interface -> [String]
getMethodNames interface = case interface of
  Slv.Untyped _ (Slv.Interface _ _ _ methods _) ->
    M.keys methods

getConstructorNames :: [Slv.TypeDecl] -> [String]
getConstructorNames typeDeclarations = case typeDeclarations of
  (td : tds) -> case td of
    Slv.Untyped _ Slv.ADT{ Slv.adtconstructors } ->
      let constructorNames = (\(Slv.Untyped _ (Slv.Constructor name _ _)) -> name) <$> adtconstructors
          nextNames = getConstructorNames tds
      in  constructorNames ++ nextNames

    _ ->
      getConstructorNames tds

  [] ->
    []

instance Optimizable Slv.AST Opt.AST where
  optimize env ast = do
    let globalVars         = mapMaybe Slv.getExpName (Slv.aexps ast)
        globalMethods      = concatMap getMethodNames $ Slv.ainterfaces ast
        globalConstructors = getConstructorNames $ Slv.atypedecls ast
        -- TODO: also generate freevars for imports and rename freeVars env in globalVars
        env' = env { freeVars = globalVars ++ globalMethods ++ globalConstructors ++ ["$"] }

    imports    <- mapM (optimize env') $ Slv.aimports ast
    exps       <- mapM (optimize env') $ Slv.aexps ast
    typeDecls  <- mapM (optimize env') $ Slv.atypedecls ast
    interfaces <- mapM (optimize env') $ Slv.ainterfaces ast
    instances  <- mapM (optimize env') $ Slv.ainstances ast

    defs <- getTopLevelExps
    resetTopLevelExps

    return $ Opt.AST { Opt.aimports    = imports
                     , Opt.aexps       = defs ++ exps
                     , Opt.atypedecls  = typeDecls
                     , Opt.ainterfaces = interfaces
                     , Opt.ainstances  = instances
                     , Opt.apath       = Slv.apath ast
                     }


typingToStr :: Slv.Typing -> String
typingToStr (Slv.Untyped _ t) = case t of
  Slv.TRSingle n -> n

  Slv.TRComp n _ -> if "." `isInfixOf` n then tail $ dropWhile (/= '.') n else n

  Slv.TRTuple ts -> "Tuple_" <> show (length ts)

buildTypeStrForPlaceholder :: [Type] -> String
buildTypeStrForPlaceholder ts = intercalate "_" $ getTypeHeadName <$> ts

getTypeHeadName :: Type -> String
getTypeHeadName t = case t of
  TVar (TV n _)   ->
    n

  TCon (TC n _) _ -> case n of
    "()"          ->
      "Unit"

    "(,)"         ->
      "Tuple_2"

    "(,,)"        ->
      "Tuple_3"

    "(,,,)"       ->
      "Tuple_4"

    "(,,,,)"      ->
      "Tuple_5"

    "(,,,,,)"     ->
      "Tuple_6"

    "(,,,,,,)"    ->
      "Tuple_7"

    "(,,,,,,,)"   ->
      "Tuple_8"

    "(,,,,,,,,)"  ->
      "Tuple_9"

    "(,,,,,,,,,)" ->
      "Tuple_10"

    _             ->
      n

  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    getTypeHeadName tl <> "_arr_" <> getTypeHeadName tr

  TApp l _  ->
    getTypeHeadName l

  TRecord fields _ ->
    let fields'   = M.map getTypeHeadName fields
        fieldsStr = intercalate "_" $ uncurry (++) <$> M.toList fields'
    in  "Record" <> "_" <> fieldsStr


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
optimizeTable :: Slv.Table -> Opt.Table
optimizeTable table =
  let env       = Env { freeVars = [], freeVarExclusion = [], stillTopLevel = True, lifted = M.empty }
      optimized = mapM (optimize env) table
  in  evalState optimized initialOptimizationState
