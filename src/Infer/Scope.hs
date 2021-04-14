{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.Scope where

import Infer.Env
import AST.Solved
import Infer.Infer
import qualified Data.Set as S
import qualified Data.Map as M
import Infer.Type
import Control.Monad
import Error.Error
import Control.Monad.Except
import Control.Exception


type InScope = S.Set String
type Accesses = S.Set (String, Exp)

-- Map of all names a given top level expression needs to run.
-- Say we have:
-- fn = (x) => x + var1 + var2
-- We'd have a dependency entry for fn like this:
-- M.fromList [("fn", S.fromList ["var1", "var2"])]
type Dependencies = M.Map String (S.Set (String, Exp))

checkAST :: Env -> AST -> Infer ()
checkAST env ast = do
  let initialNamesInScope = S.fromList (M.keys $ envVars env) <> S.fromList (M.keys $ envMethods env)
      exps                = aexps ast
  checkExps env initialNamesInScope M.empty exps

checkExps :: Env -> InScope -> Dependencies -> [Exp] -> Infer ()
checkExps _ _ _ [] = return ()
checkExps env globalScope dependencies (e:es) = do
  let globalScope'   = extendScope globalScope e

  collectedAccesses <- collect globalScope' S.empty e

  catchError (verifyScope env collectedAccesses globalScope dependencies e) pushError

  let dependencies' = extendDependencies collectedAccesses dependencies e
  checkExps env globalScope' dependencies' es


verifyScope :: Env -> Accesses -> InScope -> Dependencies -> Exp -> Infer ()
verifyScope env globalAccesses globalScope dependencies exp =
  if isFunction exp then
    return ()
  else do
    foldM (verifyScope' env globalScope dependencies) () globalAccesses

verifyScope' :: Env -> InScope -> Dependencies -> () -> (String, Exp) -> Infer ()
verifyScope' env globalScope dependencies _ access@(nameToVerify, Solved _ area _) =
  if nameToVerify `S.member` globalScope then do
    case M.lookup nameToVerify dependencies of
      Just names -> foldM_ (verifyScope' env globalScope (removeAccessFromDeps access dependencies)) () names

      Nothing    ->
        if nameToVerify `S.member` globalScope then
          return ()
        else
          throwError $ InferError (UnboundVariable nameToVerify) (Context (envCurrentPath env) area [])

    return ()
  else
    throwError $ InferError (UnboundVariable nameToVerify) (Context (envCurrentPath env) area [])

removeAccessFromDeps :: (String, Exp) -> Dependencies -> Dependencies
removeAccessFromDeps access = M.map (S.filter (/= access))

extendScope :: InScope -> Exp -> InScope
extendScope inScope exp = case getExpName exp of
  Just name -> S.insert name inScope
  Nothing   -> inScope

extendDependencies :: Accesses -> Dependencies -> Exp -> Dependencies
extendDependencies globalAccesses dependencies exp = case getExpName exp of
  Just name -> M.insert name globalAccesses dependencies

  Nothing   -> dependencies

isFunction :: Exp -> Bool
isFunction exp = case exp of
  Solved t _ _ -> isFunctionType t
  _            -> False


collect :: InScope -> InScope -> Exp -> Infer Accesses
collect globalScope localScope solvedExp@(Solved _ area e) = case e of
  TemplateString exps -> do
    globalNamesAccessed <- mapM (collect globalScope localScope) exps
    return $ foldr S.union S.empty globalNamesAccessed

  Var name ->
    if name `S.member` localScope then
      return S.empty
    else
      return $ S.singleton (name, solvedExp)

  App fn arg _ -> do
    fnGlobalNamesAccessed <- collect globalScope localScope fn
    argGlobalNamesAccessed <- collect globalScope localScope arg
    return $ fnGlobalNamesAccessed <> argGlobalNamesAccessed

  Abs (Solved _ _ name) body -> do
    let localScope' = S.insert name localScope
    collectFromBody globalScope localScope' body

    where
      collectFromBody :: InScope -> InScope -> [Exp] -> Infer Accesses
      collectFromBody _ _ []                        = return S.empty
      collectFromBody globalScope localScope (e:es) = do
        let localScope' = extendScope localScope e
        access <- collect globalScope localScope' e
        next   <- collectFromBody globalScope localScope' es
        return $ access <> next

  If cond truthy falsy -> do
    condAccesses   <- collect globalScope localScope cond
    truthyAccesses <- collect globalScope localScope truthy
    falsyAccesses  <- collect globalScope localScope falsy

    return $ condAccesses <> truthyAccesses <> falsyAccesses

  Assignment name exp -> collect globalScope (S.insert name localScope) exp

  TypedExp exp _ -> collect globalScope localScope exp

  Export exp -> collect globalScope localScope exp

  Access record fieldAccessor -> do
    recordAccesses <- collect globalScope localScope record
    fieldAccessorAccesses <- collect globalScope localScope fieldAccessor
    return $ recordAccesses <> fieldAccessorAccesses

  Where exp iss -> do
    expAccess   <- collect globalScope localScope exp
    issAccesses <- mapM (collectFromIs globalScope localScope) iss
    let issAccesses' = foldr S.union S.empty issAccesses
    return $ expAccess <> issAccesses'

  TupleConstructor exps -> do
    accesses <- mapM (collect globalScope localScope) exps
    return $ foldr S.union S.empty accesses

  ListConstructor items -> do
    listItemAccesses <- mapM (collectFromListItem globalScope localScope) items
    return $ foldr S.union S.empty listItemAccesses

  Record fields -> do
    fieldAccesses <- mapM (collectFromField globalScope localScope) fields
    return $ foldr S.union S.empty fieldAccesses

  Placeholder _ exp -> collect globalScope localScope exp

  _ -> return S.empty


collectFromField :: InScope -> InScope -> Field -> Infer Accesses
collectFromField globalScope localScope (Solved _ _ field) = case field of
  Field (name, exp) -> collect globalScope localScope exp
  FieldSpread exp   -> collect globalScope localScope exp

collectFromListItem :: InScope -> InScope -> ListItem -> Infer Accesses
collectFromListItem globalScope localScope (Solved _ _ li) = case li of
  ListItem exp   -> collect globalScope localScope exp
  ListSpread exp -> collect globalScope localScope exp

collectFromIs :: InScope -> InScope -> Is -> Infer Accesses
collectFromIs globalScope localScope (Solved _ _ (Is pat exp)) = do
  let patternScope = buildPatternScope pat
      localScope'  = localScope <> patternScope
  collect globalScope localScope' exp

buildPatternScope :: Pattern -> S.Set String
buildPatternScope (Solved _ _ pat) = case pat of
  PVar name             -> S.singleton name
  PCtor _ pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PRecord fieldPatterns -> foldr S.union S.empty $ buildPatternScope <$> M.elems fieldPatterns
  PList pats            -> foldr S.union S.empty $ buildPatternScope <$> pats
  PTuple pats           -> foldr S.union S.empty $ buildPatternScope <$> pats
  PSpread pat           -> buildPatternScope pat
  _                     -> S.empty

