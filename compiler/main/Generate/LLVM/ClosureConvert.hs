{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
module Generate.LLVM.ClosureConvert where

import qualified Control.Monad.State           as MonadState
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.List
import           AST.Core
import           Infer.Type
import Data.Maybe
import Debug.Trace
import Text.Show.Pretty
import Explain.Location
import qualified Utils.Types as Types
import qualified Utils.Hash as Hash
import qualified Data.ByteString.Lazy.Char8    as BLChar8


data State
  = State { count :: Int, topLevel :: [Exp] }

data Env
  = Env
  { freeVars :: [String]
  , freeVarExclusion :: [String]
  , dictsInScope :: [String]
  -- ^ Closured names that are reassigned. So if we find something in the higher scope, the inner function should not skip it but create a param for it
  , stillTopLevel :: Bool
  , lifted :: M.Map String (String, [Exp])
  -- ^ the key is the initial name, and then we have (lifted name, args to partially apply)
  , allocatedMutations :: [String]
  -- ^ mutations in scope currently. If a mutation is already in scope, an assignment should not allocate it on the heap as it's already there.
  , mutationsInScope :: [String]
  -- ^ all mutations that will happen in a function and its inner functions
  , moduleHash :: String
  }
  deriving(Eq, Show)

initialOptimizationState :: State
initialOptimizationState = State { count = 0, topLevel = [] }

type Convert a = forall m . MonadState.MonadState State m => m a


numbers :: [String]
numbers = show <$> [0 ..]

generateLiftedName :: Env -> String -> Convert String
generateLiftedName env originalName = do
  let hashedName = addHashToName (moduleHash env) originalName
  s@(State count _) <- MonadState.get
  let index = numbers !! count
  let name = hashedName ++ "$lifted$" ++ index
  MonadState.put s { count = count + 1 }
  return name

resetTopLevelExps :: Convert ()
resetTopLevelExps = do
  s@(State _ _) <- MonadState.get
  MonadState.put s { topLevel = [] }

addTopLevelExp :: Exp -> Convert ()
addTopLevelExp exp = do
  s@(State _ topLevel) <- MonadState.get
  MonadState.put s { topLevel = topLevel ++ [exp] }

getTopLevelExps :: Convert [Exp]
getTopLevelExps = do
  State _ topLevel <- MonadState.get
  return topLevel

addGlobalFreeVar :: String -> Env -> Env
addGlobalFreeVar fv env =
  env { freeVars = fv : freeVars env }

addVarExclusion :: String -> Env -> Env
addVarExclusion var env =
  env { freeVarExclusion = var : freeVarExclusion env }

addVarExclusions :: [String] -> Env -> Env
addVarExclusions vars env =
  env { freeVarExclusion = vars ++ freeVarExclusion env }

addLiftedLambda :: String -> String -> [Exp] -> Env -> Env
addLiftedLambda originalName liftedName args env =
  env { lifted = M.insert originalName (liftedName, args) $ lifted env }


findFreeVars :: Env -> Exp -> Convert [(String, Exp)]
findFreeVars env exp = do
  fvs <- case exp of
    Typed _ _ _ (Var "+" _) ->
      return []

    Typed _ _ _ (Var "++" _) ->
      return []

    Typed _ _ _ (Var "-" _) ->
      return []

    Typed _ _ _ (Var "*" _) ->
      return []

    Typed _ _ _ (Var "/" _) ->
      return []

    Typed _ _ _ (Var "&" _) ->
      return []

    Typed _ _ _ (Var "|" _) ->
      return []

    Typed _ _ _ (Var "^" _) ->
      return []

    Typed _ _ _ (Var "~" _) ->
      return []

    Typed _ _ _ (Var "<<" _) ->
      return []

    Typed _ _ _ (Var ">>" _) ->
      return []

    Typed _ _ _ (Var ">>>" _) ->
      return []

    Typed _ _ _ (Var "==" _) ->
      return []

    Typed _ _ _ (Var "!=" _) ->
      return []

    Typed _ _ _ (Var "!" _) ->
      return []

    Typed _ _ _ (Var ">" _) ->
      return []

    Typed _ _ _ (Var "<" _) ->
      return []

    Typed _ _ _ (Var ">=" _) ->
      return []

    Typed _ _ _ (Var "<=" _) ->
      return []

    Typed _ _ _ (Var "&&" _) ->
      return []

    Typed _ _ _ (Var "||" _) ->
      return []

    Typed _ _ _ (Var "%" _) ->
      return []

    -- field access should not be registered as a free var
    Typed _ _ _ (Var ('.' : _) _) ->
      return []

    Typed _ _ _ (Var n _) -> do
      var' <- convert env exp
      let liftedVars = lifted env
      case M.lookup n liftedVars of
        Just (_, vars) ->
          return $ map (\v@(Typed _ _ _ (Var n' _)) -> (n', v)) vars

        _ ->
          return [(n, var')]

    Typed _ _ _ (Definition params body) -> do
      vars <- findFreeVarsInBody env body
      return $ filter (\(varName, _) -> varName `notElem` (getValue <$> params)) vars

    Typed _ _ _ (Call f args) -> do
      fFreeVars   <- findFreeVars env f
      argFreeVars <- concat <$> mapM (findFreeVars env) args
      return $ fFreeVars ++ argFreeVars

    Typed _ _ _ (Do exps) ->
      findFreeVarsInBody env exps

    Typed _ _ _ (If cond truthy falsy) -> do
      condFreeVars   <- findFreeVars env cond
      truthyFreeVars <- findFreeVars env truthy
      falsyFreeVars  <- findFreeVars env falsy
      return $ condFreeVars ++ truthyFreeVars ++ falsyFreeVars

    Typed _ _ _ (TupleConstructor exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Typed _ _ _ (Access record field) -> do
      recordVars <- findFreeVars env record
      fieldVars  <- findFreeVars env field
      return $ recordVars ++ fieldVars

    Typed _ _ _ (ListConstructor exps) -> do
      vars <- mapM (findFreeVars env . getListItemExp) exps
      return $ concat vars

    Typed _ _ _ (Where whereExp iss) -> do
      expVars     <- findFreeVars env whereExp
      issFreeVars <- findFreeVarsInBranches env iss
      return $ expVars ++ issFreeVars

    Typed qt area _ (Assignment n exp) -> do
      expVars <- findFreeVars env exp
      return $ expVars ++ [(n, Typed qt area [] (Var n False))]

    -- TODO: Check that we still need this
    -- Answer: I think we do as at the time we check for free vars we did not inject the
    -- method call yet.
    Typed (_ :=> t) area _ (Literal (LNum _)) -> case t of
      TVar _ -> do
        let dictName = "$Number$" <> Types.buildTypeStrForPlaceholder [t]
        if dictName `notElem` freeVars env then
          return [(dictName, Typed ([] :=> tVar "dict") area [] (Var dictName False))]
        else
          return []

      _ ->
        return []

    Typed _ area _ (Placeholder ph exp) -> do
      (placeholderVars, excludeVars) <- case ph of
        (ClassRef interface crpNodes True _, ts) -> do
          let dictName = "$" <> interface <> "$" <> ts
              crpDictNames = S.toList $ S.fromList $ dictName : collectCRPFreeVars crpNodes
              crpFvs = (\dictName -> (dictName, Typed ([] :=> tVar "dict") area [] (Var dictName False))) <$> crpDictNames
          return (crpFvs, [])

        (ClassRef interface _ False True, ts) -> do
          let dictName = "$" <> interface <> "$" <> ts
          return ([(dictName, Typed ([] :=> tVar "dict") area [] (Var dictName False))], [])

        (MethodRef interface methodName True, ts) -> do
          let dictName = "$" <> interface <> "$" <> ts
          return ([(dictName, Typed ([] :=> tVar "dict") area [] (Var dictName False))], [methodName])

        _ ->
          return ([], [])

      expVars <- case ph of
        -- If it's a resolved method, it is accessed from the global scope
        (MethodRef _ _ False, _) ->
          return []

        _ ->
          findFreeVars env exp

      return $ filter (\(varName, _) -> varName `notElem` excludeVars) $ placeholderVars ++ expVars
      where
        collectCRPFreeVars :: [ClassRefPred] -> [String]
        collectCRPFreeVars crps =
          crps >>= (\(CRPNode interface ts var crps') ->
                      if var then
                        "$" <> interface <> "$" <> ts : collectCRPFreeVars crps'
                      else
                        collectCRPFreeVars crps'
                   )

    Typed _ _ _ (Record fields) -> do
      fvs <- mapM findFreeVarsInField fields
      return $ concat fvs
      where
        findFreeVarsInField :: Field -> Convert [(String, Exp)]
        findFreeVarsInField field = case field of
          Typed _ _ _ (Field (_, exp)) ->
            findFreeVars env exp

          Typed _ _ _ (FieldSpread exp) ->
            findFreeVars env exp

    _ ->
      return []

  let globalVars = freeVars env ++ M.keys (lifted env)
  let fvs' = M.toList $ M.fromList fvs

  return $ filter (\(varName, _) -> varName `notElem` globalVars || varName `elem` freeVarExclusion env) fvs'


findFreeVarsInBody :: Env -> [Exp] -> Convert [(String, Exp)]
findFreeVarsInBody env exps = case exps of
  (e : es) -> case e of
    Typed qt area _ (Assignment name exp) -> do
      fvs <-
        if isFunctionType (getQualified qt) then
          findFreeVars (addGlobalFreeVar name env) exp
        else
          findFreeVars env exp
      nextFVs <- findFreeVarsInBody (addGlobalFreeVar name env) es
      -- nextFVs <- findFreeVarsInBody env es
      if name `elem` freeVarExclusion env || name `elem` allocatedMutations env then
        return $ fvs ++ nextFVs ++ [(name, Typed qt area [] (Var name False))]
      else
        return $ fvs ++ nextFVs

    _ -> do
      fvs     <- findFreeVars env e
      nextFVs <- findFreeVarsInBody env es
      return $ fvs ++ nextFVs

  [] ->
    return []


findFreeVarsInBranches :: Env -> [Is] -> Convert [(String, Exp)]
findFreeVarsInBranches env iss = case iss of
  (is : next) -> do
    branchVars <- findFreeVarsInBranch env is
    nextVars   <- findFreeVarsInBranches env next
    return $ branchVars ++ nextVars

  [] ->
    return []

findFreeVarsInBranch :: Env -> Is -> Convert [(String, Exp)]
findFreeVarsInBranch env is = case is of
  Typed _ _ _ (Is pat exp) -> do
    let patternVars = getPatternVars pat
    expVars <- findFreeVars env exp
    return $ filter (\(varName, _) -> varName `notElem` patternVars) expVars


getPatternVars :: Pattern -> [String]
getPatternVars (Typed _ _ _ pat) = case pat of
  PVar n ->
    [n]

  PCon _ pats ->
    concatMap getPatternVars pats

  PRecord fields ->
    concatMap getPatternVars $ M.elems fields

  PList pats ->
    concatMap getPatternVars pats

  PTuple pats ->
    concatMap getPatternVars pats

  PSpread pat' ->
    getPatternVars pat'

  _ ->
    []


findMutationsInExp :: [String] -> Exp -> [String]
findMutationsInExp params exp = case exp of
  Typed _ _ _ (Assignment n e) | n `elem` params ->
    n : findMutationsInExp params e

  Typed _ _ _ (Assignment _ e) ->
    findMutationsInExp params e

  Typed _ _ _ (Call fn args) ->
    let fnMutations  = findMutationsInExp params fn
        argMutations = concat $ findMutationsInExp params <$> args
    in  fnMutations ++ argMutations

  Typed _ _ _ (Definition _ body) ->
    findMutationsInBody params body

  Typed _ _ _ (Access rec _) ->
    findMutationsInExp params rec

  Typed _ _ _ (ListConstructor items) ->
    concat $ findMutationsInExp params . getListItemExp <$> items

  Typed _ _ _ (TupleConstructor items) ->
    concat $ findMutationsInExp params <$> items

  Typed _ _ _ (Record fields) ->
    concat $ findMutationsInExp params . getFieldExp <$> fields

  Typed _ _ _ (If cond truthy falsy) ->
    findMutationsInExp params cond
    ++ findMutationsInExp params truthy
    ++ findMutationsInExp params falsy

  Typed _ _ _ (Do exps) ->
    findMutationsInBody params exps

  Typed _ _ _ (Where e iss) ->
    findMutationsInExp params e
    ++ concat (findMutationsInExp params . getIsExpression <$> iss)

  Typed _ _ _ (Placeholder _ e) ->
    findMutationsInExp params e

  _ ->
    []




findMutationsInBody :: [String] -> [Exp] -> [String]
findMutationsInBody params body = case body of
  exp : next ->
    findMutationsInExp params exp ++ findMutationsInBody params next

  [] ->
    []


findAllMutationsInExp :: [String] -> Exp -> [String]
findAllMutationsInExp params exp = case exp of
  Typed _ _ _ (Assignment n e) | n `elem` params ->
    n : findAllMutationsInExp params e

  Typed _ _ _ (Assignment _ e) ->
    findAllMutationsInExp params e

  Typed _ _ _ (Call fn args) ->
    let fnMutations  = findAllMutationsInExp params fn
        argMutations = concat $ findAllMutationsInExp params <$> args
    in  fnMutations ++ argMutations

  Typed _ _ _ (Definition params' body) ->
    findAllMutationsInExps (params ++ (getValue <$> params')) body

  Typed _ _ _ (Access rec _) ->
    findAllMutationsInExp params rec

  Typed _ _ _ (ListConstructor items) ->
    concat $ findAllMutationsInExp params . getListItemExp <$> items

  Typed _ _ _ (TupleConstructor items) ->
    concat $ findAllMutationsInExp params <$> items

  Typed _ _ _ (Record fields) ->
    concat $ findAllMutationsInExp params . getFieldExp <$> fields

  Typed _ _ _ (If cond truthy falsy) ->
    findAllMutationsInExp params cond
    ++ findAllMutationsInExp params truthy
    ++ findAllMutationsInExp params falsy

  Typed _ _ _ (Do exps) ->
    findAllMutationsInExps params exps

  Typed _ _ _ (Where e iss) ->
    findAllMutationsInExp params e
    ++ concat (findAllMutationsInExp params . getIsExpression <$> iss)

  Typed _ _ _ (Placeholder _ e) ->
    findAllMutationsInExp params e

  _ ->
    []


findAllMutationsInExps :: [String] -> [Exp] -> [String]
findAllMutationsInExps params exps = case exps of
  exp@(Typed _ _ _ (Assignment n _)) : next ->
    findAllMutationsInExp params exp ++ findAllMutationsInExps (n : params) next

  exp : next ->
    findAllMutationsInExp params exp ++ findAllMutationsInExps params next

  [] ->
    []


class Convertable a b where
  convert :: Env -> a -> Convert b


removeDictsNotInScope :: Env -> [(String, Exp)] -> [(String, Exp)]
removeDictsNotInScope env =
  filter ((\varName -> varName `elem` dictsInScope env || head varName /= '$') . fst)

-- At this point it's no longer top level and all functions encountered must be lifted
convertBody :: [String] -> Env -> [Exp] -> Convert [Exp]
convertBody exclusionVars env body = case body of
  [] ->
    return []

  (exp : es) -> case exp of
    Typed _ _ _ (Assignment name abs@(Typed _ _ _ (Definition _ _))) -> do
      fvs  <- findFreeVars (addGlobalFreeVar name env) abs
      let withoutFreeVarDictsNotInScope = removeDictsNotInScope env fvs
      exp' <- convertDefinition (addVarExclusions exclusionVars env) name [] withoutFreeVarDictsNotInScope abs
      next <- convertBody (name : exclusionVars) env es
      -- next <- convertBody (name : exclusionVars) (addGlobalFreeVar name env) es
      return $ exp' : next

    abs@(Typed _ _ _ (Definition _ _)) -> do
      exp' <- convert (addVarExclusions exclusionVars env) abs
      next <- convertBody exclusionVars env es
      return $ exp' : next

    Typed _ _ _ (Assignment name _) -> do
      e'   <- convert env exp
      let (e'', env') =
            -- TODO: we may need this for the above 2 as well
            if name `elem` mutationsInScope env && name `notElem` allocatedMutations env then
              (Typed (getQualType e') (getArea e') (ReferenceAllocation : getMetadata e') (getValue e'), env { allocatedMutations = name : allocatedMutations env })
            else if name `elem` mutationsInScope env && name `elem` allocatedMutations env then
              (Typed (getQualType e') (getArea e') (ReferenceStore : getMetadata e') (getValue e'), env)
            else
              (e', env)
      next <- convertBody (name : exclusionVars) env' es
      return $ e'' : next

    _ -> do
      exp' <- convert env exp
      next <- convertBody exclusionVars env es
      return $ exp' : next


collectPlaceholderParams :: Exp -> ([String], Exp)
collectPlaceholderParams ph = case ph of
  Typed _ _ _ (Placeholder (ClassRef interfaceName _ False True, ts) next) ->
    let (nextParams, nextBody) = collectPlaceholderParams next
    in  ("$" <> interfaceName <> "$" <> ts : nextParams, nextBody)

  or ->
    ([], or)


convertDefinition :: Env -> String -> [String] -> [(String, Exp)] -> Exp -> Convert Exp
convertDefinition env functionName dicts captured (Typed (ps :=> t) area metadata (Definition params body)) = do
  if stillTopLevel env then do
    let mutations = findMutationsInBody (getValue <$> params) body
    let allMutations = findAllMutationsInExps (getValue <$> params) body
    body'' <- convertBody [] env { stillTopLevel = False, allocatedMutations = allocatedMutations env ++ mutations, mutationsInScope = allMutations } body

    let placeholderParams = Typed ([] :=> tVar "dict") emptyArea [] <$> dicts
    let params' =
          placeholderParams
          ++
          (
            (\(Typed qt paramArea paramMetadata n) ->
                Typed qt paramArea (if n `elem` mutations then ReferenceParameter : paramMetadata else paramMetadata) n
            ) <$> params
          )

    let t' = foldr fn t $ tVar "dict" <$ placeholderParams

    return $ Typed (ps :=> t') area [] (Assignment functionName (Typed (ps :=> t') area metadata (Definition params' body'')))
  else do
    -- here we need to add free var parameters, lift it, and if there is any free var, replace the abs with a
    -- PAP that applies the free vars from the current scope.
    functionName' <- generateLiftedName env functionName

    let dictParams         = Typed ([] :=> tVar "dict") emptyArea [] <$> dicts
    let paramsWithFreeVars = ((\(name, Typed qt _ _ _) -> Typed qt emptyArea [ReferenceParameter | name `elem` mutationsInScope env] name) <$> captured) ++ dictParams ++ params

    body'' <- convertBody [] (addLiftedLambda functionName functionName' (snd <$> captured) env) body

    let liftedType   = foldr fn t $ (getType . snd <$> captured) ++ (tVar "dict" <$ dicts)
    let lifted'      = Typed (ps :=> liftedType) area [] (Assignment functionName' (Typed (ps :=> liftedType) area metadata (Definition paramsWithFreeVars body'')))
    let functionNode = Typed (ps :=> liftedType) area [] (Var functionName' False)

    addTopLevelExp lifted'

    if null captured then
      return $ Typed (ps :=> t) area [] (Assignment functionName functionNode)
    else
      let fvVarNodes = snd <$> captured
          fvVarNodes' =
              (\arg -> case arg of
                  Typed argQt argArea argMeta (Var n False) | n `elem` mutationsInScope env ->
                    Typed argQt argArea (ReferenceArgument : argMeta) (Var n False)

                  a ->
                    a
              ) <$> fvVarNodes
      in  return $ Typed (ps :=> t) area [] (Assignment functionName (Typed (ps :=> t) area metadata (Call functionNode fvVarNodes')))


-- When a lifted lambda is fetched from the env via Var, we apply the captured args to it
-- which results in codegen generating a PAP for it and then doing the real call with the
-- explicit args from the source language. With this we contract it back to one single call.
dedupeCallFn :: Exp -> Exp
dedupeCallFn exp = case exp of
  Typed qt area metadata (Call fn args) ->
    case fn of
      Typed _ _ _ (Call fn' args') ->
        dedupeCallFn $ Typed qt area metadata (Call fn' (args' ++ args))

      _ ->
        exp

  _ ->
    exp


instance Convertable Exp Exp where
  convert env fullExp@(Typed qt@(ps :=> t) area metadata e) = case e of
    Literal (LNum x) -> case t of
      TVar _ ->
        return $ Typed qt area metadata (
          Call
            ( Typed qt area [] (
                Placeholder
                  (MethodRef "Number" "__coerceNumber__" True, Types.buildTypeStrForPlaceholder [t])
                  (Typed qt area [] (Var "__coerceNumber__" False))
              )
            )
            [Typed qt area metadata (Literal $ LNum x)]
        )

      _ ->
        return $ Typed qt area metadata (Literal $ LNum x)

    JSExp js         -> return $ Typed qt area metadata (JSExp js)

    Call fn args -> do
      fn'    <- convert env { stillTopLevel = False } fn
      args'  <- mapM (convert env { stillTopLevel = False }) args
      return $ dedupeCallFn $ Typed qt area metadata (Call fn' args')

    Access rec field -> do
      rec'   <- convert env { stillTopLevel = False } rec
      field' <- convert env { stillTopLevel = False } field
      return $ Typed qt area metadata (Access rec' field')

    Export (Typed _ _ _ (Assignment name abs@(Typed _ _ _ Definition{}))) -> do
      fvs <- findFreeVars (addGlobalFreeVar name env) fullExp
      let withoutFreeVarDictsNotInScope = removeDictsNotInScope env fvs
      convertDefinition env name [] withoutFreeVarDictsNotInScope abs

    Assignment name abs@(Typed _ _ _ Definition{}) -> do
      fvs <- findFreeVars (addGlobalFreeVar name env) fullExp
      let withoutFreeVarDictsNotInScope = removeDictsNotInScope env fvs
      convertDefinition env name [] withoutFreeVarDictsNotInScope abs

    -- unnamed abs, we need to generate a name here
    Definition params body -> do
      body''       <- convertBody [] env body
      fvs          <- findFreeVars env fullExp
      let withoutFreeVarDictsNotInScope = removeDictsNotInScope env fvs
      functionName <- generateLiftedName env "$lambda"

      let paramsWithFreeVars =
            (
              (\(n, exp) ->
                Typed (getQualType exp) emptyArea [ReferenceParameter | n `elem` mutationsInScope env] n
              ) <$> withoutFreeVarDictsNotInScope
            ) ++ params

      let liftedType = foldr fn t (getType . snd <$> withoutFreeVarDictsNotInScope)
      let lifted'    = Typed (ps :=> liftedType) area [] (Assignment functionName (Typed (ps :=> liftedType) area metadata (Definition paramsWithFreeVars body'')))
      addTopLevelExp lifted'

      let functionNode = Typed (ps :=> liftedType) area [] (Var functionName False)

      if null withoutFreeVarDictsNotInScope then
        return functionNode
      else
        let fvVarNodes = snd <$> withoutFreeVarDictsNotInScope
            fvVarNodes' =
              (\arg -> case arg of
                  Typed argQt argArea argMeta (Var n False) | n `elem` mutationsInScope env ->
                    Typed argQt argArea (ReferenceArgument : argMeta) (Var n False)

                  a ->
                    a
              ) <$> fvVarNodes
        in  return $ Typed qt area [] (Call functionNode fvVarNodes')

    Assignment functionName ph@(Typed _ _ _ (Placeholder (ClassRef _ _ False _, _) exp)) -> do
      if stillTopLevel env then do
        let (dicts, innerExp)   = collectPlaceholderParams ph
        let typeWithPlaceholders = foldr fn t (tVar "dict" <$ dicts)
        let placeholderParams = Typed ([] :=> tVar "dict") emptyArea [] <$> dicts
        case innerExp of
          Typed _ _ _ Definition{} -> do
            convertDefinition env { stillTopLevel = True, dictsInScope = dictsInScope env ++ dicts } functionName dicts [] innerExp
          _ -> do
            innerExp' <- convert env { stillTopLevel = False, dictsInScope = dictsInScope env ++ dicts } innerExp
            return $ Typed (ps :=> typeWithPlaceholders) area metadata (Assignment functionName (Typed (ps :=> typeWithPlaceholders) area [] (Definition placeholderParams [innerExp'])))
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
        let fvsWithoutDictionary          = filter (not . (`elem` dictParams) . fst) fvs
        let withoutFreeVarDictsNotInScope = removeDictsNotInScope env fvsWithoutDictionary
        let paramsWithFreeVars            = dictParams ++ (fst <$> withoutFreeVarDictsNotInScope)
        let liftedType                    = foldr fn t ((getType . snd <$> withoutFreeVarDictsNotInScope) ++ (tVar "dict" <$ dictParams))

        functionName' <- generateLiftedName env functionName

        case innerExp of
          Typed _ _ _ (Definition _ _) -> do
            convertDefinition env { dictsInScope = dictsInScope env ++ dictParams } functionName dictParams withoutFreeVarDictsNotInScope innerExp

          _ -> do
            innerExp' <- convert (addLiftedLambda functionName functionName' (Typed ([] :=> tVar "dict") emptyArea [] . (`Var` False) <$> paramsWithFreeVars) env { dictsInScope = dictsInScope env ++ dictParams }) innerExp
            let placeholderParams = Typed ([] :=> tVar "dict") emptyArea [] <$> dictParams
            let fvParams = (\(n, exp) -> Typed (getQualType exp) emptyArea [] n) <$> withoutFreeVarDictsNotInScope
            let paramsWithFreeVars' = placeholderParams ++ fvParams
            let lifted = Typed (ps :=> liftedType) area [] (Assignment functionName' (Typed (ps :=> liftedType) area [] (Definition paramsWithFreeVars' [innerExp'])))
            addTopLevelExp lifted
            let functionNode = Typed (ps :=> liftedType) area [] (Var functionName' False)

            return $ Typed qt area metadata (Assignment functionName functionNode)

    Assignment name exp -> do
      let env' =
            if stillTopLevel env then
              let mutations = findMutationsInBody [] [exp]
                  allMutations = findAllMutationsInExps [] [exp]
              in  env { stillTopLevel = False, allocatedMutations = allocatedMutations env ++ mutations, mutationsInScope = allMutations }
            else
              env
      exp' <- convert env' exp
      return $ Typed qt area metadata (Assignment name exp')

    Export exp -> do
      convert env exp

    NameExport name ->
      return $ Typed qt area metadata (NameExport name)

    Var name isConstructor -> case M.lookup name (lifted env) of
      Just (newName, capturedArgs) -> do
        let capturedArgs' =
              (\arg -> case arg of
                  Typed argQt argArea argMeta (Var n False) | n `elem` mutationsInScope env ->
                    Typed argQt argArea (ReferenceArgument : argMeta) (Var n False)

                  a ->
                    a
              ) <$> capturedArgs
        return $ Typed qt area metadata (Call (Typed qt area [] (Var newName isConstructor)) capturedArgs')

      Nothing ->
        return $ Typed qt area metadata (Var name isConstructor)

    ListConstructor items -> do
      items' <- mapM (convert env) items
      return $ Typed qt area metadata (ListConstructor items')

    TupleConstructor exps -> do
      exps' <- mapM (convert env) exps
      return $ Typed qt area metadata (TupleConstructor exps')

    Record fields -> do
      fields' <- mapM (convert env { stillTopLevel = False }) fields
      return $ Typed qt area metadata (Record fields')

    If cond truthy falsy -> do
      cond'   <- convert env { stillTopLevel = False } cond
      truthy' <- convert env { stillTopLevel = False } truthy
      falsy'  <- convert env { stillTopLevel = False } falsy
      return $ Typed qt area metadata (If cond' truthy' falsy')

    Do exps -> do
      exps' <- convertBody [] env { stillTopLevel = False } exps
      return $ Typed qt area metadata (Do exps')

    Where exp iss -> do
      exp' <- convert env { stillTopLevel = False } exp
      iss' <- mapM (convert env { stillTopLevel = False }) iss
      return $ Typed qt area metadata (Where exp' iss')

    Extern qt name originalName -> do
      return $ Typed qt area metadata (Extern qt name originalName)

    Placeholder (placeholderRef, ts) exp -> do
      exp'            <- convert env exp
      placeholderRef' <- convertPlaceholderRef placeholderRef
      let converted = Typed qt area metadata (Placeholder (placeholderRef', ts) exp')
      -- case findPlaceholderExp (trace ("PH Exp:\n" <> ppShow (findPlaceholderExp converted)<>"\nlifted:\n"<> ppShow (lifted env)) converted) of
      -- case findPlaceholderExp converted of
      case findPlaceholderExp exp of
        -- Typed _ _ _ (Call (Typed _ _ _ (Var n False)) _) ->
        Typed _ _ _ (Var n False) ->
          case M.lookup n (lifted env) of
            Just (_, capturedVars) ->
              let capturedVars' =
                    mapMaybe
                      (\case
                          Typed _ _ _ (Var n False) ->
                            Just n

                          _ ->
                            Nothing
                      )
                      capturedVars
              in  return $ cleanUpCapturedPlaceholders env capturedVars' converted
            _ ->
              return converted

        _ ->
          return converted
      -- return $ Typed qt area metadata (Placeholder (placeholderRef', ts) exp')

    _ ->
      return fullExp


cleanUpCapturedPlaceholders :: Env -> [String] -> Exp -> Exp
cleanUpCapturedPlaceholders env capturedVars exp = case exp of
  Typed qt area metadata (Placeholder ref@(ClassRef interfaceName _ True _, ts) e) ->
    let argName = "$" <> interfaceName <> "$" <> ts
    -- in  if argName `elem` capturedVars && argName `elem` dictsInScope env then
    in  if argName `elem` capturedVars && argName `elem` dictsInScope env then
          cleanUpCapturedPlaceholders env capturedVars e
        else
          Typed qt area metadata (Placeholder ref (cleanUpCapturedPlaceholders env capturedVars e))

  or ->
    or


findPlaceholderExp :: Exp -> Exp
findPlaceholderExp exp = case exp of
  Typed _ _ _ (Placeholder (ClassRef _ _ True _, _) e) ->
    findPlaceholderExp e

  or ->
    or


convertPlaceholderRef :: PlaceholderRef -> Convert PlaceholderRef
convertPlaceholderRef phr = case phr of
  ClassRef cls ps call var -> do
    ps'  <- mapM convertClassRefPred ps
    return $ ClassRef cls ps' call var

  MethodRef cls mtd call -> do
    return $ MethodRef cls mtd call


convertClassRefPred :: ClassRefPred -> Convert ClassRefPred
convertClassRefPred (CRPNode cls ts var ps) = do
  ps'  <- mapM convertClassRefPred ps
  return $ CRPNode cls ts var ps'


instance Convertable Typing Typing where
  convert env (Untyped area metadata typing) = case typing of
    TRSingle name ->
      return $ Untyped area metadata $ TRSingle name

    TRComp name typings -> do
      typings' <- mapM (convert env) typings
      return $ Untyped area metadata $ TRComp name typings'

    TRArr left right -> do
      left'  <- convert env left
      right' <- convert env right
      return $ Untyped area metadata $ TRArr left' right'

    TRRecord fields base -> do
      fields' <- mapM (convert env) fields
      base'   <- mapM (convert env) base
      return $ Untyped area metadata $ TRRecord fields' base'

    TRTuple typings -> do
      typings' <- mapM (convert env) typings
      return $ Untyped area metadata $ TRTuple typings'

    TRConstrained constraints typing -> do
      constraints' <- mapM (convert env) constraints
      typing'      <- convert env typing
      return $ Untyped area metadata $ TRConstrained constraints' typing'

instance Convertable ListItem ListItem where
  convert env (Typed qt area metadata item) = case item of
    ListItem exp -> do
      exp' <- convert env exp
      return $ Typed qt area metadata $ ListItem exp'

    ListSpread exp -> do
      exp' <- convert env exp
      return $ Typed qt area metadata $ ListSpread exp'

instance Convertable Field Field where
  convert env (Typed qt area metadata item) = case item of
    Field (name, exp) -> do
      exp' <- convert env exp
      return $ Typed qt area metadata $ Field (name, exp')

    FieldSpread exp -> do
      exp' <- convert env exp
      return $ Typed qt area metadata $ FieldSpread exp'

instance Convertable Is Is where
  convert env (Typed qt area metadata (Is pat exp)) = do
    pat' <- convert env pat
    exp' <- convert env exp
    return $ Typed qt area metadata (Is pat' exp')

instance Convertable Pattern Pattern where
  convert env (Typed qt area metadata pat) = case pat of
    PVar name ->
      return $ Typed qt area metadata $ PVar name

    PAny ->
      return $ Typed qt area metadata PAny

    PCon name pats -> do
      pats' <- mapM (convert env) pats
      return $ Typed qt area metadata $ PCon name pats'

    PNum num ->
      return $ Typed qt area metadata $ PNum num

    PStr str ->
      return $ Typed qt area metadata $ PStr str

    PChar c ->
      return $ Typed qt area metadata $ PChar c

    PBool bool ->
      return $ Typed qt area metadata $ PBool bool

    PRecord pats -> do
      pats' <- mapM (convert env) pats
      return $ Typed qt area metadata $ PRecord pats'

    PList pats -> do
      pats' <- mapM (convert env) pats
      return $ Typed qt area metadata $ PList pats'

    PTuple pats -> do
      pats' <- mapM (convert env) pats
      return $ Typed qt area metadata $ PTuple pats'

    PSpread pat -> do
      pat' <- convert env pat
      return $ Typed qt area metadata $ PSpread pat'

instance Convertable TypeDecl TypeDecl where
  convert env (Untyped area metadata typeDecl) = case typeDecl of
    adt@ADT{} -> do
      ctors <- mapM convertConstructors $ adtconstructors adt
      return $ Untyped area metadata $ ADT { adtname         = adtname adt
                                           , adtparams       = adtparams adt
                                           , adtconstructors = ctors
                                           , adtexported     = adtexported adt
                                           }

    alias@Alias{} -> do
      aliastype <- convert env $ aliastype alias
      return $ Untyped area metadata $ Alias { aliasname     = aliasname alias
                                             , aliasparams   = aliasparams alias
                                             , aliastype     = aliastype
                                             , aliasexported = aliasexported alias
                                             }
   where
    convertConstructors :: Constructor -> Convert Constructor
    convertConstructors (Untyped a metadata (Constructor name typings t)) = do
      typings' <- mapM (convert env) typings
      return $ Untyped a metadata $ Constructor name typings' t


instance Convertable Interface Interface where
  convert env (Untyped area metadata (Interface name constraints vars methods methodTypings)) = do
    methodTypings' <- mapM (convert env) methodTypings
    return $ Untyped area metadata $ Interface name constraints vars methods methodTypings'

instance Convertable Instance Instance where
  convert env (Untyped area metadata (Instance interface constraints pred methods)) = do
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> convert env exp) methods
    return $ Untyped area metadata $ Instance interface constraints pred methods'

instance Convertable Import Import where
  convert _ (Untyped area metadata imp) = case imp of
    NamedImport names relPath absPath ->
      return $ Untyped area metadata $ NamedImport (convertImportName <$> names) relPath absPath

    DefaultImport namespace relPath absPath ->
      return $ Untyped area metadata $ DefaultImport (convertImportName namespace) relPath absPath


convertImportName :: Core String -> Core String
convertImportName (Untyped area metadata name) = Untyped area metadata name

getMethodNames :: Interface -> [String]
getMethodNames interface = case interface of
  Untyped _ _ (Interface _ _ _ methods _) ->
    M.keys methods

getConstructorNames :: [TypeDecl] -> [String]
getConstructorNames typeDeclarations = case typeDeclarations of
  (td : tds) -> case td of
    Untyped _ _ ADT{ adtconstructors } ->
      let constructorNames = (\(Untyped _ _ (Constructor name _ _)) -> name) <$> adtconstructors
          nextNames = getConstructorNames tds
      in  constructorNames ++ nextNames

    _ ->
      getConstructorNames tds

  [] ->
    []


getGlobalsFromImports :: [Import] -> [String]
getGlobalsFromImports imports = case imports of
  (imp : nextImports) -> case imp of
    Untyped _ _ (NamedImport names _ _) ->
      (getValue <$> names) ++ getGlobalsFromImports nextImports

    _ ->
      getGlobalsFromImports nextImports

  [] ->
    []


defaultGlobals :: [String]
defaultGlobals =
  []


addHashToName :: String -> String -> String
addHashToName hash name =
  "__" ++ hash ++ "__" ++ name


instance Convertable AST AST where
  convert _ ast@AST{ apath = Nothing } = return ast
  convert env ast@AST{ apath = Just path } = do
    let globalVars         = mapMaybe getExpName (aexps ast) ++ defaultGlobals
        globalMethods      = concatMap getMethodNames $ ainterfaces ast
        globalConstructors = getConstructorNames $ atypedecls ast
        globalsFromImports = getGlobalsFromImports $ aimports ast
        -- TODO: also generate freevars for imports and rename freeVars env in globalVars
        env' = env { freeVars = globalVars ++ globalMethods ++ globalConstructors ++ globalsFromImports ++ ["$"], moduleHash = Hash.hash (BLChar8.pack path) }

    imports    <- mapM (convert env') $ aimports ast
    exps       <- mapM (convert env') $ aexps ast
    typeDecls  <- mapM (convert env') $ atypedecls ast
    interfaces <- mapM (convert env') $ ainterfaces ast
    instances  <- mapM (convert env') $ ainstances ast

    defs <- getTopLevelExps

    return AST { aimports    = imports
               , aexps       = defs ++ exps
               , atypedecls  = typeDecls
               , ainterfaces = interfaces
               , ainstances  = instances
               , apath       = apath ast
               }

convertAST :: AST -> AST
convertAST ast =
  let env =
        Env { freeVars = []
            , freeVarExclusion = []
            , dictsInScope = []
            , stillTopLevel = True
            , lifted = M.empty
            , allocatedMutations = []
            , mutationsInScope = []
            , moduleHash = ""
            }
  in MonadState.evalState (convert env ast) initialOptimizationState
