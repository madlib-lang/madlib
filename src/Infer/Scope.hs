{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.Scope where

import           Infer.Env
import           AST.Solved
import           Infer.Infer
import           Infer.Type
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Maybe
import           Control.Monad
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import           Explain.Location
import           Debug.Trace
import           Text.Show.Pretty               ( ppShow )


type InScope = S.Set String
type Accesses = S.Set (String, Exp)

-- Map of all names a given top level expression needs to run.
-- Say we have:
-- fn = (x) => x + var1 + var2
-- We'd have a dependency entry for fn like this:
-- M.fromList [("fn", S.fromList ["var1", "var2"])]
type Dependencies = M.Map String (S.Set (String, Exp))


findAssignmentByName :: String -> [Exp] -> Maybe Exp
findAssignmentByName _    []       = Nothing
findAssignmentByName name (e : es) = case getExpName e of
  Just found -> if name == found then Just e else findAssignmentByName name es
  Nothing    -> findAssignmentByName name es


getAllTopLevelAssignments :: AST -> S.Set String
getAllTopLevelAssignments ast =
  S.fromList $ mapMaybe getExpName (aexps ast)


checkAST :: Env -> AST -> Infer ()
checkAST env ast = do
  errs <- getErrors
  if not (null errs) then
    return ()
  else do
    let initialNamesInScope  = S.fromList (M.keys $ envVars env) <> S.fromList (M.keys $ envMethods env)
        exps                 = aexps ast
        methods              = concat $ getInstanceMethods <$> ainstances ast
        topLevelAssignements = getAllTopLevelAssignments ast
    checkExps env ast topLevelAssignements initialNamesInScope M.empty (methods ++ exps)

checkExps :: Env -> AST -> S.Set String -> InScope -> Dependencies -> [Exp] -> Infer ()
checkExps _   _   _                   _           _            []       = return ()
checkExps env ast topLevelAssignments globalScope dependencies (e : es) = do
  let globalScope' = extendScope globalScope e

  collectedAccesses <- collect env topLevelAssignments (getExpName e) [] Nothing globalScope' S.empty e

  catchError (verifyScope env collectedAccesses globalScope' dependencies e) pushError

  let shouldBeTypedOrAbove =
        if isMethod env e then
          S.empty
        else
          S.filter
            (\(name, exp) ->
              let isTyped = maybe False isTypedExp (findAssignmentByName name (aexps ast))
              in  name `notElem` globalScope' && not isTyped && not (isTypeOrNameExport exp)
            )
            collectedAccesses

  generateShouldBeTypedOrAboveErrors env shouldBeTypedOrAbove

  let dependencies' = extendDependencies collectedAccesses dependencies e
  checkExps env ast topLevelAssignments globalScope' dependencies' es


generateShouldBeTypedOrAboveErrors :: Env -> S.Set (String, Exp) -> Infer ()
generateShouldBeTypedOrAboveErrors env = foldM_
  (\_ (name, exp) -> pushError
    $ CompilationError (ShouldBeTypedOrAbove name) (Context (envCurrentPath env) (getArea exp) (envBacktrace env))
  )
  ()


shouldSkip :: Env -> Exp -> Bool
shouldSkip env e = isMethod env e || case e of
  Untyped _ _ ->
    True

  Typed _ _ (Assignment _ shouldHaveAbs) ->
    hasAbs shouldHaveAbs

  Typed _ _ (Export (Typed _ _ (Assignment _ shouldHaveAbs))) ->
    hasAbs shouldHaveAbs

  Typed _ _ (TypedExp (Typed _ _ (Assignment _ shouldHaveAbs)) _ _) ->
    hasAbs shouldHaveAbs

  Typed _ _ (TypedExp (Typed _ _ (Export (Typed _ _ (Assignment _ shouldHaveAbs)))) _ _) ->
    hasAbs shouldHaveAbs

  _ ->
    False


hasAbs :: Exp -> Bool
hasAbs e = case e of
  Typed _ _ (Placeholder _ exp) -> hasAbs exp
  Typed _ _ (Abs         _ _  ) -> True
  _                              -> False

verifyScope :: Env -> Accesses -> InScope -> Dependencies -> Exp -> Infer ()
verifyScope env globalAccesses globalScope dependencies exp =
  if shouldSkip env exp then
    return ()
  else
    foldM (verifyScope' env S.empty globalScope dependencies exp) () globalAccesses

verifyScope' :: Env -> S.Set String -> InScope -> Dependencies -> Exp -> () -> (String, Exp) -> Infer ()
verifyScope' _ _ _ _ (Untyped _ _) _ _ =
  return ()

verifyScope' env verified globalScope dependencies originExp@(Typed _ originArea _) _ access@(nameToVerify, Typed _ area@(Area loc _) _)
  = if nameToVerify `S.member` verified then
      return ()
    else if nameToVerify `S.member` globalScope then
      case M.lookup nameToVerify dependencies of
        Just names ->
          foldM_ (verifyScope' env (verified <> S.singleton nameToVerify) globalScope dependencies originExp) () names

        Nothing    -> return ()
    else do
      currentErrors <- getErrors
      let isErrorReported = any
            (\case
              CompilationError (UnboundVariable n) (Context fp _ _) -> n == nameToVerify && envCurrentPath env == fp
              _ -> False
            )
            currentErrors
      if isErrorReported
        then return ()
        else throwError $ CompilationError (NotInScope nameToVerify loc)
                                           (Context (envCurrentPath env) originArea (envBacktrace env))

verifyScope' _ _ _ _ _ _ _ =
  return ()

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


isMethod :: Env -> Exp -> Bool
isMethod env (Untyped _ _)  = False
isMethod env (Typed _ _ e) = case e of
  Var n          -> Just True == (M.lookup n (envMethods env) >> return True)
  Assignment n _ -> Just True == (M.lookup n (envMethods env) >> return True)
  _              -> False



{-|
  collect takes care of a few things:
    - collect all top level accesses
    - look for recursive names
    - look for illegal shadowing

  To look for recursive names, nameToFind and foundNames is used. So whenever we encounter an assignment,
  we pass deeper that name as being the one to find, if we then find an access to it ( Var ), we check that
  it was found already ( which means that there was a lambda in between, making it legal ).

  To find illegal shadowing, we start with all topLevelAssignments and eventually the currentTopLevelAssignment
  if the current top level expression we collect from is an assignment. Then everytime we get to an assignment,
  we validate that it is not in the topLevelAssignments.


  NB: In case of Abs, we used to just extend topLevelAssignments with the name of the parameter, so that we also catch
  shadowing of parameters. This has been removed so that for closures we can now directly reassign a parameter.
-}
collect
  :: Env -> S.Set String -> Maybe String -> [String] -> Maybe String -> InScope -> InScope -> Exp -> Infer Accesses
collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope solvedExp
  = case solvedExp of
    (Typed tipe area (TemplateString exps)) -> do
      globalNamesAccessed <- mapM
        (collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope)
        exps
      return $ foldr S.union S.empty globalNamesAccessed

    (Typed tipe area (Var ('.' : _))) ->
      return S.empty

    (Typed tipe area (Var name))      -> do
      case nameToFind of
        Just n -> when
          (n == name && notElem n foundNames && not (isMethod env solvedExp))
          (throwError $ CompilationError (RecursiveVarAccess name) (Context (envCurrentPath env) area []))
        Nothing -> return ()
      if name `S.member` localScope then return S.empty else return $ S.singleton (name, solvedExp)

    (Typed tipe area (App fn arg _)) -> do
      fnGlobalNamesAccessed <- collect env
                                       topLevelAssignments
                                       currentTopLevelAssignment
                                       foundNames
                                       nameToFind
                                       globalScope
                                       localScope
                                       fn
      argGlobalNamesAccessed <- collect env
                                        topLevelAssignments
                                        currentTopLevelAssignment
                                        foundNames
                                        nameToFind
                                        globalScope
                                        localScope
                                        arg
      return $ fnGlobalNamesAccessed <> argGlobalNamesAccessed

    (Typed tipe area (Abs (Typed t _ name) body)) -> do
      let (nameToFind', foundNames') = case nameToFind of
            Just n  -> (Nothing, n : foundNames)
            Nothing -> (Nothing, foundNames)
      let localScope' = S.insert name localScope
      collectFromBody foundNames' nameToFind' globalScope localScope' body

     where
      collectFromBody :: [String] -> Maybe String -> InScope -> InScope -> [Exp] -> Infer Accesses
      collectFromBody _          _   _           _          []       = return S.empty
      collectFromBody foundNames ntf globalScope localScope (e : es) = do
        let localScope' = extendScope localScope e
        access <- collect env
                          topLevelAssignments
                          currentTopLevelAssignment
                          foundNames
                          ntf
                          globalScope
                          localScope'
                          e
        let nextFound = case getExpName e of
              Just n  -> n : foundNames
              Nothing -> foundNames
        next <- collectFromBody nextFound ntf globalScope localScope' es
        return $ access <> next

    (Typed tipe area (If cond truthy falsy)) -> do
      condAccesses <- collect env
                              topLevelAssignments
                              currentTopLevelAssignment
                              foundNames
                              nameToFind
                              globalScope
                              localScope
                              cond
      truthyAccesses <- collect env
                                topLevelAssignments
                                currentTopLevelAssignment
                                foundNames
                                nameToFind
                                globalScope
                                localScope
                                truthy
      falsyAccesses <- collect env
                               topLevelAssignments
                               currentTopLevelAssignment
                               foundNames
                               nameToFind
                               globalScope
                               localScope
                               falsy

      return $ condAccesses <> truthyAccesses <> falsyAccesses

    (Typed tipe area (Assignment name exp)) -> do
      when
        (Just name /= currentTopLevelAssignment && name `S.member` topLevelAssignments)
        (pushError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath env) area (envBacktrace env)))

      collect env topLevelAssignments currentTopLevelAssignment foundNames (Just name) globalScope localScope exp

    (Typed tipe area (TypedExp exp _ _)) ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp

    (Typed tipe area (Export exp)) ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp

    (Typed tipe area (Access record fieldAccessor)) -> do
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope record

    (Typed tipe area (Where exp iss)) -> do
      expAccess <- collect env
                           topLevelAssignments
                           currentTopLevelAssignment
                           foundNames
                           nameToFind
                           globalScope
                           localScope
                           exp
      issAccesses <- mapM
        (collectFromIs env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope)
        iss
      let issAccesses' = foldr S.union S.empty issAccesses
      return $ expAccess <> issAccesses'

    (Typed tipe area (TupleConstructor exps)) -> do
      accesses <- mapM
        (collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope)
        exps
      return $ foldr S.union S.empty accesses

    (Typed tipe area (ListConstructor items)) -> do
      listItemAccesses <- mapM
        (collectFromListItem env
                             topLevelAssignments
                             currentTopLevelAssignment
                             foundNames
                             nameToFind
                             globalScope
                             localScope
        )
        items
      return $ foldr S.union S.empty listItemAccesses

    (Typed tipe area (Record fields)) -> do
      fieldAccesses <- mapM
        (collectFromField env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope
        )
        fields
      return $ foldr S.union S.empty fieldAccesses

    (Typed tipe area (Placeholder _ exp)) ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp

    (Typed tipe area (NameExport name)) ->
      if name `S.member` globalScope then
        return S.empty
      else
        return $ S.singleton (name, solvedExp)

    (Untyped area (TypeExport name)) ->
      if name `S.member` globalScope then
        return S.empty
      else
        return $ S.singleton (name, solvedExp)

    (Typed _ area (Extern _ _ name)) -> do
      when
        (Just name /= currentTopLevelAssignment && name `S.member` topLevelAssignments)
        (pushError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath env) area (envBacktrace env)))

      return S.empty

    _ ->
      return S.empty


collectFromField
  :: Env -> S.Set String -> Maybe String -> [String] -> Maybe String -> InScope -> InScope -> Field -> Infer Accesses
collectFromField env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope (Typed _ _ field)
  = case field of
    Field (name, exp) ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp
    FieldSpread exp ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp

collectFromListItem
  :: Env -> S.Set String -> Maybe String -> [String] -> Maybe String -> InScope -> InScope -> ListItem -> Infer Accesses
collectFromListItem env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope (Typed _ _ li)
  = case li of
    ListItem exp ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp
    ListSpread exp ->
      collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope exp

collectFromIs
  :: Env -> S.Set String -> Maybe String -> [String] -> Maybe String -> InScope -> InScope -> Is -> Infer Accesses
collectFromIs env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope (Typed _ _ (Is pat exp))
  = do
    let patternScope = buildPatternScope pat
        localScope'  = localScope <> patternScope
    collect env topLevelAssignments currentTopLevelAssignment foundNames nameToFind globalScope localScope' exp

buildPatternScope :: Pattern -> S.Set String
buildPatternScope (Typed _ _ pat) = case pat of
  PVar name             -> S.singleton name
  PCon _ pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PRecord fieldPatterns -> foldr S.union S.empty $ buildPatternScope <$> M.elems fieldPatterns
  PList   pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PTuple  pats          -> foldr S.union S.empty $ buildPatternScope <$> pats
  PSpread pat           -> buildPatternScope pat
  _                     -> S.empty

