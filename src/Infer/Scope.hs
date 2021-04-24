{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.Scope where

import           Infer.Env
import           AST.Solved
import           Infer.Infer
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Infer.Type
import           Control.Monad
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import           Control.Exception
import           Text.Show.Pretty
import           Debug.Trace


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
checkExps _   _           _            []       = return ()
checkExps env globalScope dependencies (e : es) = do
  let globalScope' = extendScope globalScope e

  collectedAccesses <- collect env Nothing globalScope S.empty e

  catchError (verifyScope env collectedAccesses globalScope dependencies e) pushError

  let dependencies' = extendDependencies collectedAccesses dependencies e
  checkExps env globalScope' dependencies' es


verifyScope :: Env -> Accesses -> InScope -> Dependencies -> Exp -> Infer ()
verifyScope env globalAccesses globalScope dependencies exp = if isFunction exp
  then return ()
  else do
    foldM (verifyScope' env globalScope dependencies) () globalAccesses

verifyScope' :: Env -> InScope -> Dependencies -> () -> (String, Exp) -> Infer ()
verifyScope' env globalScope dependencies _ access@(nameToVerify, Solved _ area _) =
  if nameToVerify `S.member` globalScope
    then case M.lookup nameToVerify dependencies of
      Just names -> foldM_ (verifyScope' env globalScope (removeAccessFromDeps access dependencies)) () names

      Nothing    -> return () --throwError $ CompilationError (UnboundVariable nameToVerify) (Context (envCurrentPath env) area (envBacktrace env))
    else throwError $ CompilationError (UnboundVariable nameToVerify) (Context (envCurrentPath env) area (envBacktrace env))

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


collect :: Env -> Maybe String -> InScope -> InScope -> Exp -> Infer Accesses
collect env nameToFind globalScope localScope solvedExp@(Solved tipe area e) = case e of
  TemplateString exps -> do
    globalNamesAccessed <- mapM (collect env nameToFind globalScope localScope) exps
    return $ foldr S.union S.empty globalNamesAccessed

  Var ('.':_)  -> return S.empty
  Var name     -> do
    when (nameToFind == Just name) (throwError $ CompilationError (RecursiveVarAccess name) (Context (envCurrentPath env) area []))
    if name `S.member` localScope then return S.empty else return $ S.singleton (name, solvedExp)

  App fn arg _ -> do
    fnGlobalNamesAccessed  <- collect env nameToFind globalScope localScope fn
    argGlobalNamesAccessed <- collect env nameToFind globalScope localScope arg
    return $ fnGlobalNamesAccessed <> argGlobalNamesAccessed

  Abs (Solved t _ name) body -> do
    let nameToFind' =
          if isFunctionType tipe then
            Nothing
          else
            nameToFind
    let localScope' = S.insert name localScope
    collectFromBody nameToFind' globalScope localScope' body

   where
    collectFromBody :: Maybe String -> InScope -> InScope -> [Exp] -> Infer Accesses
    collectFromBody _ _           _          []     = return S.empty
    collectFromBody ntf globalScope localScope (e : es) = do
      let localScope' = extendScope localScope e
      access <- collect env ntf globalScope localScope' e
      next   <- collectFromBody ntf globalScope localScope' es
      return $ access <> next

  If cond truthy falsy -> do
    condAccesses   <- collect env nameToFind globalScope localScope cond
    truthyAccesses <- collect env nameToFind globalScope localScope truthy
    falsyAccesses  <- collect env nameToFind globalScope localScope falsy

    return $ condAccesses <> truthyAccesses <> falsyAccesses

  Assignment name exp -> do
    when (name `S.member` globalScope && not (S.null localScope))
         (pushError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath env) area (envBacktrace env)))

    collect env (Just name) globalScope localScope exp

  TypedExp exp _              -> collect env nameToFind globalScope localScope exp

  Export exp                  -> collect env nameToFind globalScope localScope exp

  Access record fieldAccessor -> do
    collect env nameToFind globalScope localScope record

  Where exp iss -> do
    expAccess   <- collect env nameToFind globalScope localScope exp
    issAccesses <- mapM (collectFromIs env nameToFind globalScope localScope) iss
    let issAccesses' = foldr S.union S.empty issAccesses
    return $ expAccess <> issAccesses'

  TupleConstructor exps -> do
    accesses <- mapM (collect env nameToFind globalScope localScope) exps
    return $ foldr S.union S.empty accesses

  ListConstructor items -> do
    listItemAccesses <- mapM (collectFromListItem env nameToFind globalScope localScope) items
    return $ foldr S.union S.empty listItemAccesses

  Record fields -> do
    fieldAccesses <- mapM (collectFromField env nameToFind globalScope localScope) fields
    return $ foldr S.union S.empty fieldAccesses

  Placeholder _ exp -> collect env nameToFind globalScope localScope exp

  _                 -> return S.empty


collectFromField :: Env -> Maybe String -> InScope -> InScope -> Field -> Infer Accesses
collectFromField env nameToFind globalScope localScope (Solved _ _ field) = case field of
  Field       (name, exp) -> collect env nameToFind globalScope localScope exp
  FieldSpread exp         -> collect env nameToFind globalScope localScope exp

collectFromListItem :: Env -> Maybe String -> InScope -> InScope -> ListItem -> Infer Accesses
collectFromListItem env nameToFind globalScope localScope (Solved _ _ li) = case li of
  ListItem   exp -> collect env nameToFind globalScope localScope exp
  ListSpread exp -> collect env nameToFind globalScope localScope exp

collectFromIs :: Env -> Maybe String -> InScope -> InScope -> Is -> Infer Accesses
collectFromIs env nameToFind globalScope localScope (Solved _ _ (Is pat exp)) = do
  let patternScope = buildPatternScope pat
      localScope'  = localScope <> patternScope
  collect env nameToFind globalScope localScope' exp

buildPatternScope :: Pattern -> S.Set String
buildPatternScope (Solved _ _ pat) = case pat of
  PVar name             -> S.singleton name
  PCtor _ pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PRecord fieldPatterns -> foldr S.union S.empty $ buildPatternScope <$> M.elems fieldPatterns
  PList   pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PTuple  pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PSpread pat           -> buildPatternScope pat
  _                     -> S.empty

