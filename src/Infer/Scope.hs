{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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
import Debug.Trace
import Text.Show.Pretty


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

  collectedAccesses <- check globalScope' S.empty e

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
verifyScope' env globalScope dependencies _ (nameToVerify, Solved _ area _) =
  if nameToVerify `S.member` globalScope then do
    let namesFromDependencies = M.lookup nameToVerify dependencies
    case namesFromDependencies of
      Just names -> foldM_ (verifyScope' env globalScope dependencies) () names

      Nothing    ->
        if nameToVerify `S.member` globalScope then
          return ()
        else
          throwError $ InferError (UnboundVariable nameToVerify) (Context (envCurrentPath env) area [])

    return ()
  else
    throwError $ InferError (UnboundVariable nameToVerify) (Context (envCurrentPath env) area [])

extendScope :: InScope -> Exp -> InScope
extendScope inScope exp = case getExpName exp of
  Just name -> S.insert name inScope
  Nothing   -> inScope

extendDependencies :: Accesses -> Dependencies -> Exp -> Dependencies
extendDependencies globalScope dependencies exp = case getExpName exp of
  Just name -> M.insert name globalScope dependencies

  Nothing   -> dependencies

isFunction :: Exp -> Bool
isFunction exp = case exp of
  Solved t _ _ -> isFunctionType t
  _            -> False


check :: InScope -> InScope -> Exp -> Infer Accesses
check globalScope localScope solvedExp@(Solved _ _ exp) = case exp of

  TemplateString exps -> do
    globalNamesAccessed <- mapM (check globalScope localScope) exps
    return $ foldr S.union S.empty globalNamesAccessed

  Var name ->
    if name `S.member` localScope then
      return S.empty
    else
      return $ S.singleton (name, solvedExp)

  App fn arg _ -> do
    fnGlobalNamesAccessed <- check globalScope localScope fn
    argGlobalNamesAccessed <- check globalScope localScope arg
    return $ fnGlobalNamesAccessed <> argGlobalNamesAccessed

  Abs (Solved _ _ name) body -> do
    let localScope' = S.insert name localScope
    checkBody globalScope localScope' body

    where
      checkBody :: InScope -> InScope -> [Exp] -> Infer Accesses
      checkBody _ _ []                        = return S.empty
      checkBody globalScope localScope (e:es) = do
        let localScope' = extendScope localScope e
        access <- check globalScope localScope' e
        next   <- checkBody globalScope localScope' es
        return $ access <> next

  If cond truthy falsy -> do
    condAccesses   <- check globalScope localScope cond
    truthyAccesses <- check globalScope localScope truthy
    falsyAccesses  <- check globalScope localScope falsy

    return $ condAccesses <> truthyAccesses <> falsyAccesses

  Assignment name exp -> check globalScope (S.insert name localScope) exp

  Where exp iss -> do
    expAccess   <- check globalScope localScope exp
    issAccesses <- mapM (checkIs globalScope localScope) iss
    let issAccesses' = foldr S.union S.empty issAccesses
    return $ expAccess <> issAccesses'

  TupleConstructor exps -> do
    accesses <- mapM (check globalScope localScope) exps
    return $ foldr S.union S.empty accesses

  Placeholder _ exp -> check globalScope localScope exp

  -- RECORD

  _ -> return S.empty


checkIs :: InScope -> InScope -> Is -> Infer Accesses
checkIs globalScope localScope (Solved _ _ (Is pat exp)) = do
  let patternScope = buildPatternScope pat
      localScope'  = localScope <> patternScope
  check globalScope localScope' exp

buildPatternScope :: Pattern -> S.Set String
buildPatternScope (Solved _ _ pat) = case pat of
  PVar name -> S.singleton name
  PAny -> S.empty
  PCtor _ pats -> foldr S.union S.empty $ buildPatternScope <$> pats
  PNum _ -> S.empty
  PStr _ -> S.empty
  PBool _ -> S.empty
  PCon _ -> S.empty
  PRecord fieldPatterns -> foldr S.union S.empty $ buildPatternScope <$> M.elems fieldPatterns
  PList pats -> foldr S.union S.empty $ buildPatternScope <$> pats
  PTuple pats -> foldr S.union S.empty $ buildPatternScope <$> pats
  PSpread pat -> buildPatternScope pat

