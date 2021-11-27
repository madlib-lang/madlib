{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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


data OptimizationState
  = OptimizationState { count :: Int, topLevel :: [Opt.Exp] }

data Env
  = Env
  { freeVars :: [Opt.Name]
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

    Slv.Solved _ _ (Slv.Var ">") ->
      return []

    Slv.Solved _ _ (Slv.Var "<") ->
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
              -- nextFVs <- findFreeVarsInBody env es
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

    Slv.Solved _ _ (Slv.If cond truthy falsy) -> do
      condFreeVars   <- findFreeVars env cond
      truthyFreeVars <- findFreeVars env truthy
      falsyFreeVars  <- findFreeVars env falsy
      return $ condFreeVars ++ truthyFreeVars ++ falsyFreeVars

    Slv.Solved _ _ (Slv.TupleConstructor exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

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

    Slv.Solved _ area (Slv.Placeholder ph exp) -> do
      (placeholderVars, excludeVars) <- case ph of
        (Slv.ClassRef interface _ _ True, ts) ->
          let tsStr = buildTypeStrForPlaceholder ts
              dictName = "$" <> interface <> "$" <> tsStr
          in  return ([(dictName, Opt.Optimized (tVar "dict") area (Opt.Var dictName))], [])

        (Slv.MethodRef interface methodName True, ts) -> do
          let tsStr    = buildTypeStrForPlaceholder ts
              dictName = "$" <> interface <> "$" <> tsStr
          return ([(dictName, Opt.Optimized (tVar "dict") area (Opt.Var dictName))], [methodName])

        _ ->
          return ([], [])
      expVars <- findFreeVars env exp
      return $ filter (\(varName, _) -> varName `notElem` excludeVars) $ placeholderVars ++ expVars

    _ ->
      return []

  let globalVars = freeVars env ++ M.keys (lifted env)
  -- TODO: that line here fucks up with placeholders
  let fvs' = M.toList $ M.fromList fvs

  return $ filter (\(varName, _) -> varName `notElem` globalVars) fvs'

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
optimizeBody :: Env -> [Slv.Exp] -> Optimize [Opt.Exp]
optimizeBody env body = case body of
  [] ->
    return []

  (exp : es) -> case exp of
    Slv.Solved _ _ (Slv.TypedExp (Slv.Solved qt@(_ :=> t) area (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body)))) _ _) -> do
      exp' <- optimizeAbs env name abs
      next <- optimizeBody (addGlobalFreeVar name env) es
      return $ exp' : next

    Slv.Solved qt@(_ :=> t) area (Slv.Assignment name abs@(Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body))) -> do
      exp' <- optimizeAbs env name abs
      next <- optimizeBody (addGlobalFreeVar name env) es
      return $ exp' : next

    Slv.Solved _ _ (Slv.Assignment name e) -> do
      e'   <- optimize env exp
      next <- optimizeBody env es
      -- next <- optimizeBody (addGlobalFreeVar name env) es
      return $ e' : next

    _ -> do
      exp' <- optimize env exp
      next <- optimizeBody env es
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
collectPlaceholderParams ph =
  let (params, body) = collectPlaceholderParams' ph
  in  (params, body)
  -- in  (S.toList $ S.fromList params, body)

collectPlaceholderParams' :: Slv.Exp -> ([String], Slv.Exp)
collectPlaceholderParams' ph = case ph of
  Slv.Solved _ _ (Slv.Placeholder (Slv.ClassRef interfaceName _ _ True, ts) next) ->
    let (nextParams, nextBody) = collectPlaceholderParams' next
        tsStr = buildTypeStrForPlaceholder ts
    in  ("$" <> interfaceName <> "$" <> tsStr : nextParams, nextBody)

  or ->
    ([], or)

collectAppArgs :: Slv.Exp -> (Slv.Exp, [Slv.Exp])
collectAppArgs app = case app of
  Slv.Solved _ _ (Slv.App next arg _) ->
    let (nextFn, nextArgs) = collectAppArgs next
    in  (nextFn, nextArgs <> [arg])

  -- Slv.Solved t area (Slv.Placeholder (Slv.ClassRef interface _ True _, ts) next) ->
  --   let (nextFn, nextArgs) = collectAppArgs next
  --       dictName           = "$" <> interface <> "$" <> buildTypeStrForPlaceholder ts
  --   in (nextFn, Slv.Solved t area (Slv.Var dictName) : nextArgs)

  b ->
    (b, [])

optimizeAbs :: Env -> String -> Slv.Exp -> Optimize Opt.Exp
optimizeAbs env functionName abs@(Slv.Solved (_ :=> t) area _) = do
  let isTopLevel = stillTopLevel env
  if isTopLevel then do
    let (params, body') = collectAbsParams abs
    body'' <- optimizeBody env { stillTopLevel = False } body'

    -- Hacky for now
    let paramTypes = getParamTypes t
    let t' =
          if length paramTypes < length params then
            tVar "a" `fn` t
          else
            t

    return $ Opt.Optimized t' area $ TopLevelAbs functionName params body''
  else do
    -- here we need to add free var parameters, lift it, and if there is any free var, replace the abs with a
    -- PAP that applies the free vars from the current scope.
    let (params, body') = collectAbsParams abs
    fvs           <- findFreeVars (addGlobalFreeVar functionName env) abs
    functionName' <- generateLiftedName functionName
    body''        <- optimizeBody (addLiftedLambda functionName functionName' (snd <$> fvs) env) body'

    let paramsWithFreeVars = (fst <$> fvs) ++ params

    let liftedType = foldr fn t (Opt.getType . snd <$> fvs)
    let lifted = Opt.Optimized liftedType area (Opt.TopLevelAbs functionName' paramsWithFreeVars body'')
    addTopLevelExp lifted

    let functionNode = Opt.Optimized t area (Opt.Var functionName')

    if null fvs then
      return $ Opt.Optimized t area (Opt.Assignment functionName functionNode False)
    else
      -- We need to carry types here
      let fvVarNodes = snd <$> fvs
      in  return $ Opt.Optimized t area (Opt.Assignment functionName (Opt.Optimized t area (Opt.App functionNode fvVarNodes)) False)



instance Optimizable Slv.Exp Opt.Exp where
  optimize _ (Slv.Untyped area (Slv.TypeExport name)) = return $ Opt.Untyped area (Opt.TypeExport name)
  optimize env fullExp@(Slv.Solved qt@(_ :=> t) area e) = case e of
    Slv.LNum  x           -> return $ Opt.Optimized t area (Opt.LNum x)

    Slv.LStr  x           -> return $ Opt.Optimized t area (Opt.LStr x)

    Slv.LBool x           -> return $ Opt.Optimized t area (Opt.LBool x)

    Slv.LUnit             -> return $ Opt.Optimized t area Opt.LUnit

    Slv.TemplateString es -> do
      es' <- mapM (optimize env { stillTopLevel = False }) es
      return $ Opt.Optimized t area (Opt.TemplateString es')

    Slv.JSExp js         -> return $ Opt.Optimized t area (Opt.JSExp js)

    Slv.App fn arg close -> do
      let (fn', args) = collectAppArgs fullExp
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
      return $ Opt.Optimized t area (Opt.App fn''' (extraArgs ++ args'))

    Slv.Access rec field -> do
      rec'   <- optimize env { stillTopLevel = False } rec
      field' <- optimize env { stillTopLevel = False } field
      return $ Opt.Optimized t area (Opt.Access rec' field')

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
      body''       <- optimizeBody env body'
      fvs          <- findFreeVars env fullExp
      functionName <- generateLiftedName "anonymous"

      let paramsWithFreeVars = (fst <$> fvs) ++ params

      let liftedType = foldr fn t (Opt.getType . snd <$> fvs)
      let lifted = Opt.Optimized liftedType area (Opt.TopLevelAbs functionName paramsWithFreeVars body'')
      addTopLevelExp lifted

      let functionNode = Opt.Optimized liftedType area (Opt.Var functionName)

      if null fvs then
        return functionNode
      else
        let fvVarNodes = snd <$> fvs
        in  return $ Opt.Optimized t area (Opt.App functionNode fvVarNodes)


    Slv.Assignment functionName ph@(Slv.Solved _ _ (Slv.Placeholder (placeholderRef@(Slv.ClassRef interfaceName _ False _), ts) exp)) -> do
      let tsStr = buildTypeStrForPlaceholder ts
      let isTopLevel = stillTopLevel env
      if isTopLevel then do
        let (params, innerExp)   = collectPlaceholderParams ph
        let typeWithPlaceholders = foldr fn t (tVar "dict" <$ params)
        innerExp' <- optimize env { stillTopLevel = False } innerExp
        return $ Opt.Optimized typeWithPlaceholders area $ TopLevelAbs functionName params [innerExp']
      else do
        let (dictParams, innerExp) = collectPlaceholderParams ph
        fvs           <- findFreeVars (addGlobalFreeVar functionName env) innerExp
        let fvsWithoutDictionary = filter (not . (`elem` dictParams) . fst) fvs
        let paramsWithFreeVars   = (fst <$> fvsWithoutDictionary) ++ dictParams

        functionName' <- generateLiftedName functionName
        innerExp'     <- optimize (addLiftedLambda functionName functionName' (Opt.Optimized (tVar "dict") emptyArea . Opt.Var <$> paramsWithFreeVars) env) innerExp
        -- innerExp'     <- optimize (addLiftedLambda functionName functionName' (snd <$> fvs) env) innerExp
        -- innerExp'     <- optimize (addLiftedLambda functionName functionName' (snd <$> fvs) env) innerExp

        -- Not sure about this, but for now we collect placeholders two times, so probably we should not
        -- collect them separately and just get them from the free vars.
        
        -- let paramsWithFreeVars   = (fst <$> fvs) ++ dictParams

        let liftedType = foldr fn t (Opt.getType . snd <$> fvs)
        let lifted = Opt.Optimized liftedType area (Opt.TopLevelAbs functionName' paramsWithFreeVars [innerExp'])
        addTopLevelExp lifted

        let functionNode = Opt.Optimized t area (Opt.Var functionName')


        return $ Opt.Optimized t area (Opt.Assignment functionName functionNode False)
        -- if null fvsWithoutDictionary then
        --   return $ Opt.Optimized t area (Opt.Assignment functionName functionNode False)
        -- else
        --   -- We need to carry types here
        --   let fvVarNodes = snd <$> fvsWithoutDictionary
        --   in  return $ Opt.Optimized t area (Opt.Assignment functionName (Opt.Optimized t area (Opt.App functionNode fvVarNodes)) False)
        -- if null fvs then
        --   return $ Opt.Optimized t area (Opt.Assignment functionName functionNode False)
        -- else
        --   -- We need to carry types here
        --   let fvVarNodes = snd <$> fvs
        --   in  return $ Opt.Optimized t area (Opt.Assignment functionName (Opt.Optimized t area (Opt.App functionNode fvVarNodes)) False)


      -- else do
      --   exp'            <- optimize env exp
      --   placeholderRef' <- optimizePlaceholderRef placeholderRef
      --   return $ Opt.Optimized t area (Opt.Placeholder (placeholderRef', tsStr) exp')

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


    -- TODO: Add top level info so that we can generate or not the name for the global scope
    Slv.Assignment name exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area (Opt.Assignment name exp' (stillTopLevel env))

    Slv.Export exp -> do
      optimize env exp
      -- exp' <- optimize env exp
      -- return $ Opt.Optimized t area (Opt.Export exp')

    Slv.NameExport name     -> return $ Opt.Optimized t area (Opt.NameExport name)

    Slv.Var        name     -> do
      return $ Opt.Optimized t area (Opt.Var name)
    -- Slv.Var        name     -> do
    --   case M.lookup name (lifted env) of
    --     Just (liftedName, args) ->
    --       if null args then
    --         return $ Opt.Optimized t area (Opt.Var liftedName)
    --       else
    --         return $ Opt.Optimized t area (Opt.App (Opt.Optimized t area (Opt.Var liftedName)) args)

    --     Nothing ->
    --       return $ Opt.Optimized t area (Opt.Var name)

    Slv.TypedExp exp _ scheme -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area (Opt.TypedExp exp' scheme)

    Slv.ListConstructor items -> do
      items' <- mapM (optimize env) items
      return $ Opt.Optimized t area (Opt.ListConstructor items')

    Slv.TupleConstructor exps -> do
      exps' <- mapM (optimize env) exps
      return $ Opt.Optimized t area (Opt.TupleConstructor exps')

    Slv.Record fields -> do
      fields' <- mapM (optimize env { stillTopLevel = False }) fields
      return $ Opt.Optimized t area (Opt.Record fields')

    Slv.If cond truthy falsy -> do
      cond'   <- optimize env { stillTopLevel = False } cond
      truthy' <- optimize env { stillTopLevel = False } truthy
      falsy'  <- optimize env { stillTopLevel = False } falsy
      return $ Opt.Optimized t area (Opt.If cond' truthy' falsy')

    Slv.Do exps -> do
      exps' <- optimizeBody env { stillTopLevel = False } exps --mapM (optimize env { stillTopLevel = False }) exps
      return $ Opt.Optimized t area (Opt.Do exps')

    Slv.Where exp iss -> do
      exp' <- optimize env { stillTopLevel = False } exp
      iss' <- mapM (optimize env { stillTopLevel = False }) iss
      return $ Opt.Optimized t area (Opt.Where exp' iss')

    Slv.Extern qt name originalName -> do
      return $ Opt.Optimized t area (Opt.Extern qt name originalName)

    Slv.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- optimize env exp
      placeholderRef' <- optimizePlaceholderRef placeholderRef
      let tsStr = buildTypeStrForPlaceholder ts
      return $ Opt.Optimized t area (Opt.Placeholder (placeholderRef', tsStr) exp')

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
      return $ Opt.Optimized t area $ Opt.ListItem exp'

    Slv.ListSpread exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.ListSpread exp'

instance Optimizable Slv.Field Opt.Field where
  optimize env (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.Field (name, exp) -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.Field (name, exp')

    Slv.FieldSpread exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.FieldSpread exp'

instance Optimizable Slv.Is Opt.Is where
  optimize env (Slv.Solved qt@(_ :=> t) area (Slv.Is pat exp)) = do
    pat' <- optimize env pat
    exp' <- optimize env exp
    return $ Opt.Optimized t area (Opt.Is pat' exp')

instance Optimizable Slv.Pattern Opt.Pattern where
  optimize env (Slv.Solved qt@(_ :=> t) area pat) = case pat of
    Slv.PVar name       -> return $ Opt.Optimized t area $ Opt.PVar name

    Slv.PAny            -> return $ Opt.Optimized t area Opt.PAny

    Slv.PCon name pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PCon name pats'

    Slv.PNum    num  -> return $ Opt.Optimized t area $ Opt.PNum num

    Slv.PStr    str  -> return $ Opt.Optimized t area $ Opt.PStr str

    Slv.PBool   boo  -> return $ Opt.Optimized t area $ Opt.PBool boo

    Slv.PRecord pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PRecord pats'

    Slv.PList pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PList pats'

    Slv.PTuple pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PTuple pats'

    Slv.PSpread pat -> do
      pat' <- optimize env pat
      return $ Opt.Optimized t area $ Opt.PSpread pat'

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

instance Optimizable Slv.AST Opt.AST where
  optimize env ast = do
    let globalVars    = mapMaybe Slv.getExpName (Slv.aexps ast)
        globalMethods = concatMap getMethodNames $ Slv.ainterfaces ast
        env' = env { freeVars = globalVars ++ globalMethods }

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
  TVar (TV n _)   -> n
  TCon (TC n _) _ -> case n of
    "()"          -> "Unit"
    "(,)"         -> "Tuple_2"
    "(,,)"        -> "Tuple_3"
    "(,,,)"       -> "Tuple_4"
    "(,,,,)"      -> "Tuple_5"
    "(,,,,,)"     -> "Tuple_6"
    "(,,,,,,)"    -> "Tuple_7"
    "(,,,,,,,)"   -> "Tuple_8"
    "(,,,,,,,,)"  -> "Tuple_9"
    "(,,,,,,,,,)" -> "Tuple_10"
    _             -> n
  TApp (TApp (TCon (TC "(->)" _) _) tl) tr -> getTypeHeadName tl <> "_arr_" <> getTypeHeadName tr
  TApp l _  -> getTypeHeadName l


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
optimizeTable :: Slv.Table -> Opt.Table
optimizeTable table =
  let env       = Env { freeVars = [], stillTopLevel = True, lifted = M.empty }
      optimized = mapM (optimize env) table
  in  evalState optimized initialOptimizationState
