{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Canonicalize.Canonicalize where

import           Run.Target
import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Canonicalize.JSExp
import           Canonicalize.CanonicalM
import qualified Canonicalize.Env              as Env
import           Canonicalize.Typing
import qualified Data.Map                      as Map
import qualified Data.List                     as List
import           AST.Canonical
import           Explain.Location
import           Control.Monad.Except
import qualified Data.Maybe as Maybe
import           Text.Show.Pretty
import qualified Canonicalize.EnvUtils as EnvUtils
import qualified Rock
import qualified Driver.Query as Query
import qualified Infer.Type as Ty
import qualified Text.Show.Pretty as PP
import qualified Data.Set as Set
import           Error.Warning
import           Error.Context
import           Text.Regex.TDFA ((=~))
import           Error.Error


class Canonicalizable a b where
  canonicalize :: Env.Env -> Target -> a -> CanonicalM b


mainTyping :: Src.Typing
mainTyping = Src.Source emptyArea Src.TargetAll (Src.TRArr (Src.Source emptyArea Src.TargetAll (Src.TRComp "List" [Src.Source emptyArea Src.TargetAll (Src.TRSingle "String")])) (Src.Source emptyArea Src.TargetAll (Src.TRSingle "{}")))


updateMainFunction :: Env.Env -> Target -> Area -> Src.Exp -> Src.Typing -> CanonicalM Can.Exp
updateMainFunction env target initialArea (Src.Source assignmentArea _ (Src.Assignment mainName main)) typing = do
  sc        <- typingToScheme env typing
  mainSC    <- typingToScheme env mainTyping

  when (sc /= mainSC) $ throwError $ CompilationError MainInvalidTyping (Context (Env.envCurrentPath env) initialArea)

  canTyping <- canonicalizeTyping typing
  main'     <- canonicalize env target main
  if Src.isAbs main then
    return $ Can.Canonical initialArea (Can.TypedExp (Can.Canonical assignmentArea (Can.Assignment mainName main')) canTyping sc)
  else
    return $ Can.Canonical initialArea (Can.TypedExp (Can.Canonical assignmentArea
      (Can.Assignment
        mainName
        (Can.Canonical assignmentArea (Can.Abs (Can.Canonical emptyArea "__args__") [Can.Canonical assignmentArea (Can.App main' (Can.Canonical emptyArea (Can.Var "__args__")) True)]))
    )) canTyping sc)


instance Canonicalizable Src.Exp Can.Exp where
  canonicalize env target fullExp@(Src.Source area sourceTarget e) = case e of
    Src.NamedTypedExp _ (Src.Source _ _ (Src.Export mainAssignment@(Src.Source _ _ (Src.Assignment "main" _)))) typing | Env.envIsMainModule env ->
      updateMainFunction env target area mainAssignment typing

    Src.NamedTypedExp _ mainAssignment@(Src.Source _ _ (Src.Assignment "main" _)) typing | Env.envIsMainModule env ->
      updateMainFunction env target area mainAssignment typing

    Src.TypedExp (Src.Source _ _ (Src.Export mainAssignment@(Src.Source _ _ (Src.Assignment "main" _)))) typing | Env.envIsMainModule env ->
      updateMainFunction env target area mainAssignment typing

    Src.TypedExp mainAssignment@(Src.Source _ _ (Src.Assignment "main" _)) typing | Env.envIsMainModule env -> do
      updateMainFunction env target area mainAssignment typing

    Src.Export mainAssignment@(Src.Source _ _ (Src.Assignment "main" _)) | Env.envIsMainModule env ->
      updateMainFunction env target area mainAssignment mainTyping

    Src.Assignment "main" _ | Env.envIsMainModule env ->
      updateMainFunction env target area fullExp mainTyping

    Src.LNum  x ->
      return $ Can.Canonical area (Can.LNum x)

    Src.LFloat x ->
      return $ Can.Canonical area (Can.LFloat x)

    Src.LStr  x ->
      return $ Can.Canonical area (Can.LStr x)

    Src.LChar  x ->
      return $ Can.Canonical area (Can.LChar x)

    Src.LBool x ->
      return $ Can.Canonical area (Can.LBool x)

    Src.LUnit ->
      return $ Can.Canonical area Can.LUnit

    Src.TemplateString es -> do
      es' <- mapM (canonicalize env target) (Maybe.mapMaybe cleanUp es)
      return $ Can.Canonical area (Can.TemplateString es')
        where
          cleanUp :: Src.Exp -> Maybe Src.Exp
          cleanUp exp = case exp of
            Src.Source _ _ (Src.LStr "") ->
              Nothing

            Src.Source area srcTarget (Src.LStr s) ->
              Just $ Src.Source area srcTarget (Src.LStr ("\"" <> s <> "\""))

            _ ->
              Just exp

    Src.JSExp js -> do
      pushJS js
      return $ Can.Canonical area (Can.JSExp $ filterJSExp target js)

    Src.App fn args ->
      buildApp env target area fn args

    Src.UnOp op arg ->
      buildApp env target area op [arg]

    Src.BinOp argL op argR -> case op of
      Src.Source area srcTarget (Src.Var "++") -> do
        buildApp env target area (Src.Source area srcTarget (Src.Var "mconcat")) [argL, argR]

      Src.Source area srcTarget (Src.Var "!=") | target == TLLVM -> do
        equalCheck <- buildApp env target area (Src.Source area srcTarget (Src.Var "==")) [argL, argR]
        return $ Can.Canonical area (Can.App (Can.Canonical area (Can.Var "!")) equalCheck True)

      Src.Source _ _ (Src.Var "|>") ->
        buildApp env target area argR [argL]

      _ ->
        buildApp env target area op [argL, argR]

    Src.Access rec field -> do
      rec'   <- canonicalize env target rec
      field' <- canonicalize env target field
      return $ Can.Canonical area (Can.Access rec' field')

    Src.AbsWithMultilineBody params body ->
      processAbs env target area params body

    Src.Abs params body ->
      processAbs env target area params body

    Src.Return exp ->
      canonicalize env target exp

    Src.Assignment "__EQ__" exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Assignment "==" exp')

    Src.Assignment name exp -> do
      exp' <- canonicalize env target exp
      pushNameDeclaration (Env.envExpPosition env) name
      return $ Can.Canonical area (Can.Assignment name exp')

    Src.Export exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Export exp')

    Src.NameExport name -> do
      pushNameAccess name
      return $ Can.Canonical area (Can.NameExport name)

    Src.TypeExport name -> do
      pushTypeAccess name
      EnvUtils.lookupADT env name
      return $ Can.Canonical area (Can.TypeExport name)

    Src.Var ('#' : name) -> do
      maybeFromEnv <- Rock.fetch $ Query.EnvVar name
      case maybeFromEnv of
        Just found ->
          return $ Can.Canonical area (Can.LStr found)

        Nothing ->
          return $ Can.Canonical area (Can.LStr "")

    Src.Var name -> do
      pushNameAccess name
      return $ Can.Canonical area (Can.Var name)

    Src.TypedExp exp typing -> do
      exp'    <- canonicalize env target exp
      scheme  <- typingToScheme env typing
      typing' <- canonicalizeTyping typing
      return $ Can.Canonical area (Can.TypedExp exp' typing' scheme)

    Src.NamedTypedExp n exp typing -> do
      exp'    <- canonicalize env target exp
      let expName = Can.getExpName exp'
      when (expName /= Just n) $ do
        throwError $ CompilationError (TypeAnnotationNameMismatch n (Maybe.fromMaybe "" expName)) (Context (Env.envCurrentPath env) (Can.getArea exp'))

      scheme  <- typingToScheme env typing
      typing' <- canonicalizeTyping typing
      return $ Can.Canonical area (Can.TypedExp exp' typing' scheme)

    Src.ListConstructor items -> do
      items' <- mapM (canonicalize env target) items
      return $ Can.Canonical area (Can.ListConstructor items')

    Src.TupleConstructor exps -> do
      exps' <- mapM (canonicalize env target) exps
      return $ Can.Canonical area (Can.TupleConstructor exps')

    Src.Record fields -> do
      fields' <- mapM (canonicalize env target) fields
      unless (Can.hasSpread fields') $ do
        let fieldNames = Maybe.mapMaybe Can.getFieldName fields'
        pushRecordToDerive fieldNames
      return $ Can.Canonical area (Can.Record fields')

    Src.If cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.Ternary cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.Do exps -> do
      exps' <- canonicalizeDoExps exps
      return $ Can.Canonical area (Can.Do exps')
      where
        canonicalizeDoExps :: [Src.Exp] -> CanonicalM [Can.Exp]
        canonicalizeDoExps []     = return []
        canonicalizeDoExps (e:es) = case e of
          Src.Source assignmentArea@(Area (Loc a l c) _) _ (Src.DoAssignment name action) -> do
            exp' <- canonicalize env target action
            es'  <- canonicalizeDoExps es
            let symbolArea = Area (Loc a l c) (Loc (a + length name) l (c + length name))
            let fn  = Can.Canonical emptyArea (Can.Var "chain")
            let abs = Can.Canonical (mergeAreas assignmentArea area) $ Can.Abs (Can.Canonical symbolArea name) es'
            let app = Can.Canonical (mergeAreas assignmentArea area) (Can.App (Can.Canonical area $ Can.App fn abs False) exp' True)
            return [app]

          _ -> do
            e'  <- canonicalize env target e
            es' <- canonicalizeDoExps es
            return $ e':es'

    Src.Where exp iss -> do
      exp' <- canonicalize env target exp
      iss' <- mapM (canonicalize env target) iss
      return $ Can.Canonical area (Can.Where exp' iss')

    Src.WhereAbs iss -> do
      parameterIndex <- generateParameterIndex
      let parameterName = "__W__" ++ show parameterIndex
      buildAbs
        env
        target
        area
        [Src.Source emptyArea sourceTarget parameterName]
        [Src.Source area sourceTarget (Src.Where (Src.Source emptyArea sourceTarget (Src.Var parameterName)) iss)]

    Src.JsxTag{} -> do
      canonicalizeJsxTag env target (Src.Source area sourceTarget e)

    Src.JsxAutoClosedTag name props -> do
      canonicalizeJsxTag env target (Src.Source area sourceTarget (Src.JsxTag name props []))

    Src.Parenthesized _ exp _ ->
      canonicalize env target exp

    Src.Pipe exps -> do
      parameterIndex <- generateParameterIndex
      let parameterName = "__P__" ++ show parameterIndex
      let (Area (Loc x l c) _) = area
          varPLoc              = Area (Loc x l c) (Loc (x + 3) l (c + 3))
      app <- buildApplication (Can.Canonical varPLoc (Can.Var parameterName)) exps
      return $ Can.Canonical area (Can.Abs (Can.Canonical varPLoc parameterName) [app])
     where
      buildApplication :: Can.Exp -> [Src.Exp] -> CanonicalM Can.Exp
      buildApplication prev es = case es of
        [e] -> do
          e' <- canonicalize env target e
          return $ Can.Canonical (mergeAreas (getArea prev) (Can.getArea e')) (Can.App e' prev True)

        e : es -> do
          e' <- canonicalize env target e
          let app = Can.Canonical (mergeAreas (getArea prev) (Can.getArea e')) (Can.App e' prev True)
          buildApplication app es

    Src.Dictionary items -> do
      items' <- mapM (canonicalize env target) items
      let fromListName = Env.envFromDictionaryListName env
      if '.' `elem` fromListName then do
        let namespace = takeWhile (/= '.') fromListName
            name = reverse $ takeWhile (/= '.') (reverse fromListName)
        pushNameAccess namespace
        return $ Can.Canonical area (Can.App
          (Can.Canonical area (Access (Can.Canonical area (Can.Var namespace)) (Can.Canonical area (Can.Var ('.':name)))))
          (Can.Canonical area (Can.ListConstructor items')) True)
      else do
        pushNameAccess fromListName

        return $ Can.Canonical area (Can.App
          (Can.Canonical area (Can.Var fromListName))
          (Can.Canonical area (Can.ListConstructor items')) True)

    Src.Extern typing name originalName -> do
      scheme  <- typingToScheme env typing
      return $ Can.Canonical area (Can.Extern scheme name originalName)

    Src.TypedHole ->
      return $ Can.Canonical area Can.TypedHole


processAbs :: Env.Env -> Target -> Area -> [Src.Source Src.Name] -> [Src.Exp] -> CanonicalM Can.Exp
processAbs env target area params body = do
  allAccessesBeforeAbs <- getAllAccesses
  allDeclaredBeforeAbs <- getAllDeclaredNames

  resetNameAccesses
  resetNamesDeclared

  abs'               <- buildAbs env target area params body

  allAccessesInAbs   <- getAllAccesses
  nameAccessesInAbs  <- getAllNameAccesses
  namesDeclaredInAbs <- getAllDeclaredNames

  setAccesses (allAccessesInAbs <> allAccessesBeforeAbs)
  setDeclaredNames (namesDeclaredInAbs <> allDeclaredBeforeAbs)

  let localDecls   = map (\(e, i) -> (Src.getLocalOrNotExportedAssignmentName e, i + Env.envExpPosition env)) (zip body [0..])
  let localDecls'  = map (\(Just x, i) -> (x, i)) $ filter (Maybe.isJust . fst) localDecls
  let localDecls'' = filter (\(Src.Source _ _ n, _) -> n `Set.notMember` (Set.map (\(Declared _ n') -> n') allDeclaredBeforeAbs)) localDecls'
  let unusedParams = filter (\(Src.Source _ _ n) -> n /= "_" && n `Set.notMember` nameAccessesInAbs) params
  let unusedDecls  =
        filter
          (\(Src.Source _ _ n, i) ->
            n /= "_"
            && n `Set.notMember` nameAccessesInAbs
            && n `Set.notMember` (Set.map (\(Declared _ n') -> n') $ Set.filter (\(Declared pos _) -> pos > i) namesDeclaredInAbs)
          )
          localDecls''
  let unusedDecls' = map fst unusedDecls

  allJS <- getJS

  unusedParams' <-
    if null unusedParams then
      return unusedParams
    else do
      return $ filter (not . (allJS =~) . (\(Src.Source _ _ n) -> n)) unusedParams

  unusedDecls'' <-
    if null unusedDecls' then
      return unusedDecls'
    else do
      return $ filter (not . (allJS =~) . (\(Src.Source _ _ n) -> n)) unusedDecls'

  forM_ unusedParams' $ \(Src.Source area' _ n) -> do
    pushWarning $ CompilationWarning (UnusedParameter n) (Context (Env.envCurrentPath env) area')

  forM_ unusedDecls'' $ \(Src.Source area' _ n) -> do
    pushWarning $ CompilationWarning (UnusedDeclaration n) (Context (Env.envCurrentPath env) area')

  return abs'

buildAbs :: Env.Env -> Target -> Area -> [Src.Source Src.Name] -> [Src.Exp] -> CanonicalM Can.Exp
buildAbs env target area@(Area _ (Loc a l c)) [] body = do
  body' <- mapM (\(e, i) -> canonicalize env { Env.envExpPosition = i + Env.envExpPosition env } target e) (zip body [0..])
  let param = Can.Canonical (Area (Loc (a - 1) l (c - 1)) (Loc (a - 1) l (c - 1))) "_"
  return $ Can.Canonical area (Can.Abs param body')
buildAbs env target area [Src.Source area' _ param] body = do
  body' <- mapM (\(e, i) -> canonicalize env { Env.envExpPosition = i + Env.envExpPosition env } target e) (zip body [0..])
  return $ Can.Canonical area (Can.Abs (Can.Canonical area' param) body')
buildAbs env target area (Src.Source area' _ param:xs) body  = do
  next <- buildAbs env target area xs body
  return $ Can.Canonical area (Can.Abs (Can.Canonical area' param) [next])


placeholderArgCheck :: [Src.Exp] -> CanonicalM ([Src.Exp], [Src.Source String])
placeholderArgCheck args = case args of
  (arg : next) -> case arg of
    Src.Source area target (Src.Var "$") -> do
      phIndex <- generatePlaceholderIndex
      let paramName = "__$PH" ++ show phIndex ++ "__"
      (nextArgs, params) <- placeholderArgCheck next
      return (Src.Source area target (Src.Var paramName) : nextArgs, Src.Source area target paramName : params)

    _ -> do
      (nextArgs, params) <- placeholderArgCheck next
      return (arg : nextArgs, params)

  [] ->
    return ([], [])


buildApp :: Env.Env -> Target -> Area -> Src.Exp -> [Src.Exp] -> CanonicalM Can.Exp
buildApp env target area@(Area _ (Loc a l c)) f [] = do
  f' <- canonicalize env target f
  let arg' = Can.Canonical (Area (Loc (a - 1) l (c - 1)) (Loc (a - 1) l (c - 1))) Can.LUnit
  return $ Can.Canonical area (Can.App f' arg' True)
buildApp env target area f args = do
  (args', wrapperPlaceholderParams) <- placeholderArgCheck args
  let (droppable, _) =
        span (\case
                (Src.Source _ _ (Src.Var n)) ->
                  "__$PH" `List.isPrefixOf` n

                _ ->
                  False
            ) $ reverse args'
  let canDrop = length droppable

  let args'' = (reverse . drop canDrop . reverse) args'
      wrapperPlaceholderParams' = (reverse . drop canDrop . reverse) wrapperPlaceholderParams

  if null args'' then
    canonicalize env target f
  else if null wrapperPlaceholderParams' then
    buildApp' env target (length args'') (length args'') area f args''
  else
    let app = Src.Source area (Src.getSourceTarget f) (Src.App f args'')
    in  buildAbs env target area wrapperPlaceholderParams' [app]


buildApp' :: Env.Env -> Target -> Int -> Int -> Area -> Src.Exp -> [Src.Exp] -> CanonicalM Can.Exp
buildApp' env target total nth area f@Src.Source{} args = case args of
  [arg] -> do
    arg' <- canonicalize env target arg
    f'   <- canonicalize env target f
    return $ Can.Canonical area (Can.App f' arg' (total == nth))

  _ -> do
    let arg@(Src.Source area' _ _) = last args
    arg'   <- canonicalize env target arg
    subApp <- buildApp' env target total (nth - 1) area f (init args)
    return $ Can.Canonical (mergeAreas area area') (Can.App subApp arg' (total == nth))


canonicalizeJsxTag :: Env.Env -> Target -> Src.Exp -> CanonicalM Can.Exp
canonicalizeJsxTag env target exp = case exp of
  Src.Source area _ (Src.JsxTag name props children) -> do
    pushNameAccess name
    pushNameAccess "text" -- fix for now
    let Area (Loc _ l c) (Loc _ _ _) = area
    let tagFnArea = Area (Loc 0 l c) (Loc 0 l (c + length name + 2))
    let tagFnVar = Can.Canonical tagFnArea (Can.Var name)

    children' <- mapM canonicalizeJsxChild children
    propFns <- mapM
      (\(Src.Source a _ (Src.JsxProp name' exp)) -> do
        pushNameAccess name'
        let Area (Loc _ l c) (Loc _ _ _) = a
        arg <- canonicalize env target exp
        return $ Can.Canonical
          a
          (Can.ListItem $ Can.Canonical
            a
            (Can.App (Can.Canonical (Area (Loc 0 l c) (Loc 0 l (c + length name'))) (Can.Var name')) arg True)
          )
      )
      props
    let propArea     = composeArea (length name) area propFns
    let childrenArea = composeArea 0 area children'

    let props'       = Can.Canonical propArea (Can.ListConstructor propFns)
    let children''   = Can.Canonical childrenArea (Can.ListConstructor children')

    return $ Can.Canonical
      area
      (Can.App (Can.Canonical (mergeAreas tagFnArea propArea) (Can.App tagFnVar props' False)) children'' True)
    where
    composeArea :: Int -> Area -> [Can.Canonical a] -> Area
    composeArea offset (Area (Loc _ l c) _) cans = if not (null cans)
      then mergeAreas (getArea $ head cans) (getArea $ last cans)
      else Area (Loc 0 l (c + offset)) (Loc 0 l (c + offset))

    canonicalizeJsxChild :: Src.JsxChild -> CanonicalM Can.ListItem
    canonicalizeJsxChild child = case child of
      Src.JsxChild exp -> do
        e' <- canonicalize env target exp
        return $ Can.Canonical area (Can.ListItem e')

      Src.JsxExpChild exp -> do
        e' <- canonicalize env target exp
        return $ Can.Canonical area (Can.ListItem e')

      Src.JsxSpreadChild exp -> do
        e' <- canonicalize env target exp
        return $ Can.Canonical area (Can.ListSpread e')



instance Canonicalizable Src.DictItem Can.ListItem where
  canonicalize env target (Src.Source area _ exp) = case exp of
    (Src.DictItem key value) -> do
      key' <- canonicalize env target key
      value' <- canonicalize env target value
      return $ Can.Canonical area (Can.ListItem (Can.Canonical area (Can.TupleConstructor [key', value'])))


instance Canonicalizable Src.ListItem Can.ListItem where
  canonicalize env target (Src.Source area _ item) = case item of
    Src.ListItem exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.ListItem exp'

    Src.ListSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.ListSpread exp'


instance Canonicalizable Src.Field Can.Field where
  canonicalize env target (Src.Source area _ item) = case item of
    Src.Field (name, exp) -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.Field (name, exp')

    Src.FieldShorthand name -> do
      pushNameAccess name
      return $ Can.Canonical area $ Can.Field (name, Can.Canonical area (Can.Var name))

    Src.FieldSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.FieldSpread exp'


instance Canonicalizable Src.Is Can.Is where
  canonicalize env target (Src.Source area _ (Src.Is pat exp)) = do
    pat' <- canonicalize env target pat
    exp' <- canonicalize env target exp
    return $ Can.Canonical area (Can.Is pat' exp')


instance Canonicalizable Src.Pattern Can.Pattern where
  canonicalize env target (Src.Source area _ pat) = case pat of
    Src.PVar name       -> return $ Can.Canonical area (Can.PVar name)

    Src.PAny            -> return $ Can.Canonical area Can.PAny

    Src.PCon (Src.Source _ _ name) pats -> do
      let nameToPush = if "." `List.isInfixOf` name then takeWhile (/= '.') name else name
      pushNameAccess nameToPush
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PCon name pats')

    Src.PNullaryCon (Src.Source _ _ name) -> do
      let nameToPush = if "." `List.isInfixOf` name then takeWhile (/= '.') name else name
      pushNameAccess nameToPush
      return $ Can.Canonical area (Can.PCon name [])

    Src.PNum    num  -> return $ Can.Canonical area (Can.PNum num)

    Src.PStr    str  -> return $ Can.Canonical area (Can.PStr str)

    Src.PChar    str  -> return $ Can.Canonical area (Can.PChar str)

    Src.PBool   boo  -> return $ Can.Canonical area (Can.PBool boo)

    Src.PRecord pats -> do
      pats' <- mapM (canonicalize env target) (extractPatternFields pats)
      return $ Can.Canonical area (Can.PRecord pats')

    Src.PList pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PList pats')

    Src.PTuple pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PTuple pats')

    Src.PSpread pat -> do
      pat' <- canonicalize env target pat
      return $ Can.Canonical area (Can.PSpread pat')


extractPatternFields :: [Src.PatternField] -> Map.Map Src.Name Src.Pattern
extractPatternFields pats = case pats of
  (Src.PatternField (Src.Source _ _ fieldName) pat : ps) ->
    Map.insert fieldName pat (extractPatternFields ps)

  (Src.PatternFieldShorthand (Src.Source area sourceTarget fieldName) : ps) ->
    Map.insert fieldName (Src.Source area sourceTarget (Src.PVar fieldName)) (extractPatternFields ps)

  [] ->
    mempty


instance Canonicalizable Src.Import Can.Import where
  canonicalize _ _ (Src.Source area _ imp) = case imp of
    Src.NamedImport names relPath absPath ->
      return $ Can.Canonical area (Can.NamedImport (canonicalizeName <$> names) relPath absPath)

    Src.TypeImport names relPath absPath ->
      return $ Can.Canonical area (Can.TypeImport (canonicalizeName <$> names) relPath absPath)

    Src.DefaultImport namespace relPath absPath ->
      return $ Can.Canonical area (Can.DefaultImport (canonicalizeName namespace) relPath absPath)


canonicalizeName :: Src.Source Src.Name -> Can.Canonical Can.Name
canonicalizeName (Src.Source area _ name) = Can.Canonical area name
