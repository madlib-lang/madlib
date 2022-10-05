{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Infer.Placeholder where

import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Infer.Pred
import           Infer.Unify
import           Infer.Substitute
import qualified AST.Solved                    as Slv
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Maybe                    as Maybe
import           Control.Monad.Except
import           Error.Error
import           Error.Context
import           Infer.Instantiate
import LLVM.AST.Instruction (Instruction(cleanup))
import Debug.Trace
import Text.Show.Pretty
import Run.Options


{-
Post type inference traversing of a top level expression or method. It mainly does one thing,
which is assuring consistency of generated placeholders and update. Because only after
generalization and instance resolution we have complete knowledge of the types for the placeholder
types, we need to go through the tree again to update these and generate appropriate types.

For example we may have a parameter placeholder being fully resolved like:
(Show_a) => ... becomes (Show_Integer) => ...
In that case we can completely strip it.

Or we may have multiple levels of a dictionary, in which case we simply strip the inner one as
it's already being captured in the current scope, example:
(Show_a, a) => {
  return (Show_a, a) => ...
}
In that case we can simply remove the inner one:
(Show_a) => {
  return (a) => ...
}
The codegen would then capture the usage of it via findFreeVars during closure conversion.


The main entrypoint of the module is updatePlaceholders, which mainly traverses the AST and calls
the updateMethod/ClassPlaceholder whenever a placeholder is encountered.
-}


insertVarPlaceholders :: Options -> Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
insertVarPlaceholders _ _   exp                    []       = return exp
insertVarPlaceholders options env exp@(Slv.Typed t a e) (p : ps) =
  if optInsertInstancePlaholders options then do
    ts <- getCanonicalPlaceholderTypes env p
    if isMethod env exp (p:ps)
      then case e of
        Slv.Var n _ -> do
          var <- shouldInsert env $ IsIn (predClass p) (predTypes p) Nothing
          return $ Slv.Typed t a $ Slv.Placeholder (Slv.MethodRef (predClass p) n var, ts) exp
        _ -> return exp
      else do
        insert <- shouldInsert env p

        let exp' =
              if insert then
                Slv.Typed t a $ Slv.Placeholder (Slv.ClassRef (predClass p) [] True insert, ts) exp
              else
                exp
        insertVarPlaceholders options env exp' ps
  else
    return exp


insertClassPlaceholders :: Options -> Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
insertClassPlaceholders _ _   exp []       = return exp
insertClassPlaceholders options env exp (p : ps) = do
  if optInsertInstancePlaholders options then do
    insert <- shouldInsert env p
    if not insert then
      insertClassPlaceholders options env exp ps
    else case exp of
      Slv.Typed a t (Slv.Assignment n e) ->
        let exp' = Slv.Typed a t
              $ Slv.Assignment
                  n
                  (Slv.Typed a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)
        in  insertClassPlaceholders options env exp' ps

      Slv.Typed a t (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment n e)) typing sc) ->
        let exp' = Slv.Typed a t (Slv.TypedExp 
                      (Slv.Typed a t (Slv.Assignment n (Slv.Typed a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)))
                      typing
                      sc
                    )
        in  insertClassPlaceholders options env exp' ps

      Slv.Typed a t (Slv.Export (Slv.Typed a' t' (Slv.Assignment n e))) ->
        let exp' = Slv.Typed a t $ Slv.Export
              (Slv.Typed
                a'
                t'
                (Slv.Assignment
                  n
                  (Slv.Typed a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)
                )
              )
        in  insertClassPlaceholders options env exp' ps

      Slv.Typed a t (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment n e)))) typing sc) ->
        let exp' = Slv.Typed a t (Slv.TypedExp
                      (Slv.Typed a t (Slv.Export
                        (Slv.Typed a t (Slv.Assignment n (Slv.Typed a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e))
                      )))
                      typing
                      sc
                    )
        in  insertClassPlaceholders options env exp' ps

      _ -> return exp
  else
    return exp

any' :: [a] -> (a -> Bool) -> Bool
any' = flip any



removePlaceholders :: [Int] -> Int -> Slv.Exp -> Slv.Exp
removePlaceholders indicesToRemove index exp = case exp of
  Slv.Typed qt area (Slv.Placeholder ref@(Slv.ClassRef _ _ True _, _) e) ->
    if index `elem` indicesToRemove then
      removePlaceholders indicesToRemove (index + 1) e
    else
      Slv.Typed qt area (Slv.Placeholder ref (removePlaceholders indicesToRemove (index + 1) e))

  _ ->
    exp

cleanUpDeletedVarPlaceholders :: Env -> Slv.Exp -> Slv.Exp
cleanUpDeletedVarPlaceholders env exp = case exp of
  Slv.Typed _ _ (Slv.Placeholder (Slv.ClassRef _ _ True _, _) _) ->
    case getPlaceholderExp exp of
      Slv.Typed _ _ (Slv.Var name False) ->
        let psToRemove = Maybe.fromMaybe [] (M.lookup name (envPlaceholdersToDelete env))
        in  removePlaceholders psToRemove 0 exp

      _ ->
        exp

  Slv.Typed qt area (Slv.Placeholder ref@(Slv.ClassRef _ _ False _, _) e) ->
    let e' = cleanUpDeletedVarPlaceholders env e
    in  Slv.Typed qt area (Slv.Placeholder ref e')

  Slv.Typed qt area (Slv.App fn arg isFinal) ->
    let fn'  = cleanUpDeletedVarPlaceholders env fn
        arg' = cleanUpDeletedVarPlaceholders env arg
    in  Slv.Typed qt area (Slv.App fn' arg' isFinal)

  Slv.Typed qt area (Slv.TemplateString exps) ->
    let exps' = map (cleanUpDeletedVarPlaceholders env) exps
    in  Slv.Typed qt area (Slv.TemplateString exps')

  Slv.Typed qt area (Slv.Access rec field) ->
    let rec'   = cleanUpDeletedVarPlaceholders env rec
        field' = cleanUpDeletedVarPlaceholders env field
    in  Slv.Typed qt area (Slv.Access rec' field')

  Slv.Typed qt area (Slv.Abs param body) ->
    let body' = map (cleanUpDeletedVarPlaceholders env) body
    in  Slv.Typed qt area (Slv.Abs param body')

  Slv.Typed qt area (Slv.Do body) ->
    let body' = map (cleanUpDeletedVarPlaceholders env) body
    in  Slv.Typed qt area (Slv.Do body')

  Slv.Typed qt area (Slv.Assignment name value) ->
    let value' = cleanUpDeletedVarPlaceholders env value
    in  Slv.Typed qt area (Slv.Assignment name value')

  Slv.Typed qt area (Slv.Export e) ->
    let e' = cleanUpDeletedVarPlaceholders env e
    in  Slv.Typed qt area (Slv.Export e')

  Slv.Typed qt area (Slv.TypedExp e typing sc) ->
    let e' = cleanUpDeletedVarPlaceholders env e
    in  Slv.Typed qt area (Slv.TypedExp e' typing sc)

  Slv.Typed qt area (Slv.ListConstructor items) ->
    let cleanUpListItem = \case
          Slv.Typed qt area (Slv.ListItem e) ->
            Slv.Typed qt area (Slv.ListItem (cleanUpDeletedVarPlaceholders env e))

          Slv.Typed qt area (Slv.ListSpread e) ->
            Slv.Typed qt area (Slv.ListSpread (cleanUpDeletedVarPlaceholders env e))
        items' = map cleanUpListItem items
    in  Slv.Typed qt area (Slv.ListConstructor items')

  Slv.Typed qt area (Slv.Record fields) ->
    let cleanUpField = \case
          Slv.Typed qt area (Slv.Field (n, e)) ->
            Slv.Typed qt area (Slv.Field (n, cleanUpDeletedVarPlaceholders env e))

          Slv.Typed qt area (Slv.FieldSpread e) ->
            Slv.Typed qt area (Slv.FieldSpread (cleanUpDeletedVarPlaceholders env e))
        fields' = map cleanUpField fields
    in  Slv.Typed qt area (Slv.Record fields')

  Slv.Typed qt area (Slv.TupleConstructor exps) ->
    let exps' = map (cleanUpDeletedVarPlaceholders env) exps
    in  Slv.Typed qt area (Slv.TupleConstructor exps')

  Slv.Typed qt area (Slv.If cond truthy falsy) ->
    let cond'   = cleanUpDeletedVarPlaceholders env cond
        truthy' = cleanUpDeletedVarPlaceholders env truthy
        falsy'  = cleanUpDeletedVarPlaceholders env falsy
    in  Slv.Typed qt area (Slv.If cond' truthy' falsy')

  Slv.Typed qt area (Slv.Where e iss) ->
    let cleanUpIs = \(Slv.Typed qt' area' (Slv.Is pat e')) ->
          let e'' = cleanUpDeletedVarPlaceholders env e'
          in  Slv.Typed qt' area' (Slv.Is pat e'')
        iss' = map cleanUpIs iss
        e'   = cleanUpDeletedVarPlaceholders env e
    in  Slv.Typed qt area (Slv.Where e' iss')

  -- TODO: walk down the tree
  
  or ->
    or



shouldInsert :: Env -> Pred -> Infer Bool
shouldInsert env p = do
  inst <- findInst env $ IsIn (predClass p) (predTypes p) Nothing

  let insert = case inst of
        Just (Instance (_ :=> p') _) ->
          let (_, IsIn _ withoutVars _) = removeInstanceVars p' p
          in  any isTVar withoutVars

        Nothing ->
          True
  return insert


getCanonicalPlaceholderTypes :: Env -> Pred -> Infer [Type]
getCanonicalPlaceholderTypes env p@(IsIn _ ts _) = do
  inst <- findInst env p
  return $ case inst of
    Just (Instance (_ :=> (IsIn _ ts'' _)) _) -> ts''
    Nothing -> ts


updateMethodPlaceholder :: Options -> Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateMethodPlaceholder options env _ s ph@(Slv.Typed qt@(_ :=> t) a (Slv.Placeholder (Slv.MethodRef cls method _, instanceTypes) (Slv.Typed qt' a' exp))) =
  if optInsertInstancePlaholders options then do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env (IsIn cls instanceTypes' Nothing)
    var'  <- shouldInsert env $ IsIn cls instanceTypes' Nothing -- Reconsider if the instance is fully resolved

    -- The following block serves to check that the inferred type for a method
    -- which was unified in type check with the class' scheme, is actually a
    -- type that is correct, given the actual instance's specific type.
    -- ss    <- do
    do
      maybeInst <- findInst env (IsIn cls instanceTypes' Nothing)
      case maybeInst of
        Just (Instance _ methods) -> case M.lookup method methods of
          Just methodScheme -> do
            (_ :=> mtdT) <- instantiate methodScheme
            catchError
              (match mtdT (apply s t))
              (\(CompilationError e _) ->
                throwError $ CompilationError e (Context (envCurrentPath env) a)
              )
          Nothing -> return mempty
        Nothing -> return mempty

    ps  <- catchError (byInst env $ IsIn cls instanceTypes' Nothing) (const $ return [])
    ps' <- getAllParentPreds env ps

    pushPlaceholders
      env
      (
        Slv.Typed
          (apply s qt)
          a
          (
            Slv.Placeholder
              (Slv.MethodRef cls method var', types)
              (Slv.Typed (apply s qt')a' exp)
          )
      )
      ps'
  else
    return ph


pushPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
pushPlaceholders _   exp                    []                     = return exp
pushPlaceholders env exp@(Slv.Typed qt a _) (p@(IsIn cls ts _) : ps) = do
  var <- shouldInsert env $ IsIn cls ts Nothing
  ps' <- buildClassRefPreds env cls ts
  ts' <- getCanonicalPlaceholderTypes env p

  let ph = Slv.Typed qt a (Slv.Placeholder (Slv.ClassRef cls ps' True var, ts') exp)
  pushPlaceholders env ph ps


collectPlaceholders :: Slv.Exp -> ([(Slv.PlaceholderRef, [Type])], Slv.Exp)
collectPlaceholders ph = case ph of
  Slv.Typed _ _ (Slv.Placeholder phRef next) ->
    let (nextPHRefs, nextExp) = collectPlaceholders next
    in  (phRef : nextPHRefs, nextExp)

  _ ->
    ([], ph)


getPlaceholderExp :: Slv.Exp -> Slv.Exp
getPlaceholderExp ph = case ph of
  Slv.Typed _ _ (Slv.Placeholder (Slv.ClassRef{}, _) next) ->
    getPlaceholderExp next

  found ->
    found


isNameInScope :: Maybe String -> CleanUpEnv -> Bool
isNameInScope maybeName cleanUpEnv = case maybeName of
  Just n ->
    n `elem` namesInScope cleanUpEnv

  Nothing ->
    False


updateClassPlaceholder :: Options -> Env -> CleanUpEnv -> Maybe String ->  Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateClassPlaceholder options env cleanUpEnv _ push s ph =
  if optInsertInstancePlaholders options then
    case ph of
      Slv.Typed qt a (Slv.Placeholder (Slv.ClassRef cls _ call var, instanceTypes) exp) -> do
        let instanceTypes' = apply s instanceTypes
        types <- getCanonicalPlaceholderTypes env $ IsIn cls instanceTypes' Nothing
        var'  <- shouldInsert env $ IsIn cls instanceTypes' Nothing

        ps'   <- buildClassRefPreds env cls instanceTypes'

        let newRef = (cls, instanceTypes')
        nextEnv <-
              if not call then do
                -- we need to gather parent interfaces as a constraint Applicative f => ...
                -- will generate an additional Functor f constraint and we need this one
                -- to be in the env or the clean up will not be complete.
                parents <- getAllParentPreds env [IsIn cls instanceTypes' Nothing]
                let allRefs = (\(IsIn name ts _) -> (name, ts)) <$> parents
                return $ foldr addDict cleanUpEnv allRefs
              else
                return $ addAppliedDict (cls, instanceTypes) cleanUpEnv
        exp'  <- updatePlaceholders options env nextEnv push s exp

        let wrappedExp = getPlaceholderExp ph

        let maybeName =
              case wrappedExp of
                Slv.Typed _ _ (Slv.Var n _) ->
                  Just n

                _ ->
                  Nothing

        if not call then
          -- if not var' then
          --   -- if the instance is resolved there's no point in having (IntegerDict) => ... and we can
          --   -- safely drop the dictionary.
          --   return exp'
          -- else
            return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls [] call var, instanceTypes') exp')
        else
          return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls ps' call var', types) exp')



        -- if not call then
        --   if not var' then
        --     -- if the instance is resolved there's no point in having (IntegerDict) => ... and we can
        --     -- safely drop the dictionary.
        --     return exp'
        --   -- else if newRef `elem` dictsInScope cleanUpEnv && not (isMethodDef cleanUpEnv) then do
        --   --   -- In that case we need to extend the env to express what dictionary was removed so that we can
        --   --   -- remove the dictionaries at call sites
        --   --   -- In the case of method definition, the predicates come from interface declaration and instance
        --   --   -- parents and we don't have a solution right now to dedupe these properly so we don't touch them
        --   --   -- just yet.
        --   --   return exp'
        --   else
        --     return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls [] call var, instanceTypes') exp')
        -- else if (cls, instanceTypes') `elem` appliedDicts cleanUpEnv then
        --   return exp'
        -- else
        --   return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls ps' call var', types) exp')

        -- if (newRef `elem` dictsInScope cleanUpEnv) && isNameInScope maybeName cleanUpEnv then do
        --   -- liftIO $ putStrLn $ "maybeName: " <> ppShow maybeName <> "\nisInScope: " <> ppShow (isNameInScope maybeName cleanUpEnv) <> "\n\n"
        --   -- this class ref is already in scope so we skip the placeholder
        --   return exp'
        -- else if not call then
        --   if not var' then
        --     -- if the instance is resolved there's no point in having (IntegerDict) => ... and we can
        --     -- safely drop the dictionary.
        --     return exp'
        --   else if newRef `elem` dictsInScope cleanUpEnv && not (isMethodDef cleanUpEnv) then do
        --     -- In that case we need to extend the env to express what dictionary was removed so that we can
        --     -- remove the dictionaries at call sites
        --     -- In the case of method definition, the predicates come from interface declaration and instance
        --     -- parents and we don't have a solution right now to dedupe these properly so we don't touch them
        --     -- just yet.
        --     return exp'
        --   else
        --     return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls [] call var, instanceTypes') exp')
        -- else if (cls, instanceTypes') `elem` appliedDicts cleanUpEnv then
        --   return exp'
        -- else
        --   return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls ps' call var', types) exp')


        -- if (newRef `elem` dictsInScope cleanUpEnv || call && not var') && isNameInScope maybeName cleanUpEnv then
        --   -- this class ref is already in scope so we skip the placeholder
        --   return exp'
        -- else if not call then
        --   if not var' then
        --     -- if the instance is resolved there's no point in having (IntegerDict) => ... and we can
        --     -- safely drop the dictionary.
        --     return exp'
        --   else if newRef `elem` dictsInScope cleanUpEnv && not (isMethodDef cleanUpEnv) then do
        --     -- In that case we need to extend the env to express what dictionary was removed so that we can
        --     -- remove the dictionaries at call sites
        --     -- In the case of method definition, the predicates come from interface declaration and instance
        --     -- parents and we don't have a solution right now to dedupe these properly so we don't touch them
        --     -- just yet.
        --     return exp'
        --   else
        --     return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls [] call var, instanceTypes') exp')
        -- else if (cls, instanceTypes') `elem` appliedDicts cleanUpEnv then
        --   return exp'
        -- else
        --   return $ Slv.Typed (apply s qt) a (Slv.Placeholder (Slv.ClassRef cls ps' call var', types) exp')

      _ ->
        undefined
  else
    return ph



buildClassRefPreds :: Env -> String -> [Type] -> Infer [Slv.ClassRefPred]
buildClassRefPreds env cls ts = do
  maybeInst <- findInst env $ IsIn cls ts Nothing
  instTypes <- case maybeInst of
    Just (Instance (_ :=> (IsIn _ x _)) _) ->
      return x

    Nothing ->
      return ts


  s    <- unify instTypes ts
  ps   <- catchError (byInst env $ IsIn cls ts Nothing) (const $ return [])
  pps' <- mapM (getParentPreds env) (reverse (apply s ps))

  let ps' = concat $ reverse <$> pps'

  mapM
    (\(IsIn cls' ts' _) -> do
      next <- buildClassRefPreds env cls' ts'
      ts'' <- getCanonicalPlaceholderTypes env $ IsIn cls' ts' Nothing
      var  <- shouldInsert env (IsIn cls' ts' Nothing)
      return $ Slv.CRPNode cls' ts'' var next
    )
    ps'




data CleanUpEnv
  = CleanUpEnv
  { isMethodDef :: Bool
  , dictsInScope :: [(String, [Type])]
  , appliedDicts :: [(String, [Type])]
  , namesInScope :: [String]
  }
  deriving(Eq, Show)

addDict :: (String, [Type]) -> CleanUpEnv -> CleanUpEnv
addDict dict env =
  env { dictsInScope = dict : dictsInScope env }

addAppliedDict :: (String, [Type]) -> CleanUpEnv -> CleanUpEnv
addAppliedDict dict env =
  env { appliedDicts = dict : appliedDicts env }

addName :: String -> CleanUpEnv -> CleanUpEnv
addName name env =
  env { namesInScope = name : namesInScope env }



updatePlaceholdersForExpList :: Options -> Env -> CleanUpEnv -> Bool -> Substitution -> [Slv.Exp] -> Infer [Slv.Exp]
updatePlaceholdersForExpList options env cleanUpEnv push s exps = case exps of
  (e : es) -> case e of
    Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _) -> do
      let cleanUpEnv' = addName name cleanUpEnv
      next <- updatePlaceholdersForExpList options env cleanUpEnv' push s es
      e' <- updatePlaceholders options env cleanUpEnv' push s e
      return (e' : next)

    Slv.Typed _ _ (Slv.Assignment name _) -> do
      let cleanUpEnv' = addName name cleanUpEnv
      next <- updatePlaceholdersForExpList options env cleanUpEnv' push s es
      e' <- updatePlaceholders options env cleanUpEnv' push s e
      return (e' : next)

    _ -> do
      next <- updatePlaceholdersForExpList options env cleanUpEnv push s es
      e'   <- updatePlaceholders options env cleanUpEnv push s e
      return (e' : next)

  [] ->
    return []


updatePlaceholders :: Options -> Env -> CleanUpEnv -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updatePlaceholders _ _ _ _ _ fullExp@(Slv.Untyped _ _)   = return fullExp
updatePlaceholders options env cleanUpEnv push s fullExp@(Slv.Typed qt a e) = case e of
  Slv.Placeholder (ref, _) _ -> case ref of
    Slv.MethodRef{} ->
      updateMethodPlaceholder options env push s fullExp

    _  ->
      updateClassPlaceholder options env cleanUpEnv Nothing push s fullExp


  Slv.App abs arg final -> do
    abs' <- updatePlaceholders options env cleanUpEnv { appliedDicts = [] } push s abs
    arg' <- updatePlaceholders options env cleanUpEnv { appliedDicts = [] } push s arg
    return $ Slv.Typed (apply s qt) a $ Slv.App abs' arg' final

  Slv.Abs (Slv.Typed paramType paramArea param) es -> do
    -- Once we encountered an Abs we processed all the instance placeholders and we can then
    -- strip the inner placeholders.
    es' <- updatePlaceholdersForExpList options env cleanUpEnv { isMethodDef = False } push s es
    let param' = Slv.Typed (apply s paramType) paramArea param
    return $ Slv.Typed (apply s qt) a $ Slv.Abs param' es'

  Slv.Do exps -> do
    exps' <- updatePlaceholdersForExpList options env cleanUpEnv push s exps
    return $ Slv.Typed (apply s qt) a $ Slv.Do exps'

  Slv.Where exp iss -> do
    exp' <- updatePlaceholders options env cleanUpEnv push s exp
    iss' <- mapM (updateIs s) iss
    return $ Slv.Typed (apply s qt) a $ Slv.Where exp' iss'

  Slv.Assignment n ph@(Slv.Typed _ _ (Slv.Placeholder (Slv.ClassRef{}, _) _)) -> do
    updatedPlaceholder <- updateClassPlaceholder options env cleanUpEnv (Just n) push s ph
    return $ Slv.Typed  (apply s qt) a (Slv.Assignment n updatedPlaceholder)

  Slv.Assignment n exp -> do
    exp' <- updatePlaceholders options env cleanUpEnv push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.Assignment n exp'

  Slv.ListConstructor li -> do
    li' <- mapM (updateListItem s) li
    return $ Slv.Typed (apply s qt) a $ Slv.ListConstructor li'

  Slv.TypedExp exp typing sc -> do
    exp' <- updatePlaceholders options env cleanUpEnv push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.TypedExp exp' typing sc

  Slv.Export exp -> do
    exp' <- updatePlaceholders options env cleanUpEnv push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.Export exp'

  Slv.If econd eif eelse -> do
    econd' <- updatePlaceholders options env cleanUpEnv push s econd
    eif'   <- updatePlaceholders options env cleanUpEnv push s eif
    eelse' <- updatePlaceholders options env cleanUpEnv push s eelse
    return $ Slv.Typed (apply s qt) a $ Slv.If econd' eif' eelse'

  Slv.TupleConstructor es -> do
    es' <- mapM (updatePlaceholders options env cleanUpEnv push s) es
    return $ Slv.Typed (apply s qt) a $ Slv.TupleConstructor es'

  Slv.TemplateString es -> do
    es' <- mapM (updatePlaceholders options env cleanUpEnv push s) es
    return $ Slv.Typed (apply s qt) a $ Slv.TemplateString es'

  Slv.Access rec field -> do
    rec'   <- updatePlaceholders options env cleanUpEnv push s rec
    field' <- updatePlaceholders options env cleanUpEnv push s field
    return $ Slv.Typed (apply s qt) a $ Slv.Access rec' field'

  Slv.Record fields -> do
    fields' <- mapM (updateField s) fields
    return $ Slv.Typed (apply s qt) a $ Slv.Record fields'

  _ -> return $ Slv.Typed (apply s qt) a e

  where
    updateIs :: Substitution -> Slv.Is -> Infer Slv.Is
    updateIs s (Slv.Typed qt@(ps :=> _) a is) = case is of
      Slv.Is pat exp -> do
        exp' <- updatePlaceholders options env cleanUpEnv push s exp
        return $ Slv.Typed (apply s qt) a $ Slv.Is (updatePattern s ps pat) exp'

    updatePattern :: Substitution -> [Pred] -> Slv.Pattern -> Slv.Pattern
    updatePattern s preds pat@(Slv.Typed (_ :=> t) _ _) =
      let ps = selectPredsForType preds t
      in  case pat of
        Slv.Typed (ps' :=> t') area (Slv.PCon n pats) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PCon n (updatePattern s preds <$> pats))

        Slv.Typed (ps' :=> t') area (Slv.PRecord fields) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PRecord (updatePattern s preds <$> fields))

        Slv.Typed (ps' :=> t') area (Slv.PList items) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PList (updatePattern s preds <$> items))

        Slv.Typed (ps' :=> t') area (Slv.PTuple items) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PTuple (updatePattern s preds <$> items))

        Slv.Typed (ps' :=> t') area (Slv.PSpread pat) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PSpread (updatePattern s preds pat))

        Slv.Typed (ps' :=> t') area (Slv.PVar n) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PVar n)

        Slv.Typed (ps' :=> t') area p ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area p


    updateListItem :: Substitution -> Slv.ListItem -> Infer Slv.ListItem
    updateListItem s (Slv.Typed t area li) = case li of
      Slv.ListItem e ->
        Slv.Typed t area . Slv.ListItem <$> updatePlaceholders options env cleanUpEnv push s e

      Slv.ListSpread e ->
        Slv.Typed t area . Slv.ListSpread <$> updatePlaceholders options env cleanUpEnv push s e

    updateField :: Substitution -> Slv.Field -> Infer Slv.Field
    updateField s (Slv.Typed t area field) = case field of
      Slv.Field       (n, e) -> Slv.Typed t area . Slv.Field . (n, ) <$> updatePlaceholders options env cleanUpEnv push s e
      Slv.FieldSpread e      -> Slv.Typed t area . Slv.FieldSpread <$> updatePlaceholders options env cleanUpEnv push s e



getPredClassNames :: [Pred] -> S.Set String
getPredClassNames ps =
  let clsNames = predClass <$> ps
  in  S.fromList clsNames

isMethod :: Env -> Slv.Exp -> [Pred] -> Bool
isMethod _ (Slv.Untyped _ _) _  = False
isMethod env (Slv.Typed _ _ e) ps = case e of
  Slv.Var n _ ->
    case M.lookup n (envMethods env) of
      Nothing ->
        False

      Just (Forall _ (ps' :=> _)) ->
        getPredClassNames ps' == getPredClassNames ps

  _ ->
    False
