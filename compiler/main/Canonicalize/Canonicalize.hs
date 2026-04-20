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
import qualified Data.Set                      as Set
import           AST.Canonical
import           Explain.Location
import           Control.Monad.Except
import qualified Data.Maybe                    as Maybe
import           Text.Show.Pretty
import qualified Canonicalize.EnvUtils         as EnvUtils
import qualified Rock
import qualified Driver.Query                  as Query
import qualified Infer.Type                    as Ty
import qualified Text.Show.Pretty              as PP
import qualified Data.Set                      as Set
import           Error.Warning
import           Error.Context
import           Text.Regex.TDFA ((=~))
import           Error.Error
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Data.Char                       as Char


charParser :: ReadS String
charParser = ReadP.readP_to_S $ ReadP.many $ ReadP.readS_to_P Char.readLitChar


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

    Src.LByte  x -> do
      when ((read x :: Int) > 2^8 - 1) $
        throwError $ CompilationError (ByteOutOfBounds x) (Context (Env.envCurrentPath env) area)
      return $ Can.Canonical area (Can.LByte x)

    Src.LShort  x -> do
      when ((read x :: Int) > 2^31 - 1) $
        throwError $ CompilationError (ShortOutOfBounds x) (Context (Env.envCurrentPath env) area)
      return $ Can.Canonical area (Can.LShort x)

    Src.LInt  x -> do
      when ((read x :: Integer) > 2^63 - 1) $
        throwError $ CompilationError (IntOutOfBounds x) (Context (Env.envCurrentPath env) area)
      return $ Can.Canonical area (Can.LInt x)

    Src.LFloat x ->
      if "_f" `List.isSuffixOf` x then
        return $ Can.Canonical area (Can.LFloat (init $ init x))
      else
        return $ Can.Canonical area (Can.LFloat x)

    Src.LStr  x ->
      return $ Can.Canonical area (Can.LStr x)

    Src.LChar ('\\':'u':'{':chars) -> do
      let char' = head $ fst $ last $ charParser ("\\" ++ show (read ('0':'x':init chars) :: Int))
      return $ Can.Canonical area (Can.LChar char')

    Src.LChar ('\\':'u':chars) -> do
      let char' = head $ fst $ last $ charParser ("\\x" ++ chars)
      return $ Can.Canonical area (Can.LChar char')

    Src.LChar char -> do
      let char' = head $ fst $ last $ charParser char
      return $ Can.Canonical area (Can.LChar char')

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

            _ ->
              Just exp

    Src.JSExp js -> do
      pushJS js
      return $ Can.Canonical area (Can.JSExp $ filterJSExp target js)

    Src.App fn args ->
      buildApp env target area fn args

    Src.UnOp (Src.Source area _ (Src.Var "unary-minus")) (Src.Source _ _ (Src.LByte _)) ->
      throwError $ CompilationError NegatedByte (Context (Env.envCurrentPath env) area)

    Src.UnOp op arg ->
      buildApp env target area op [arg]

    Src.BinOp argL op argR -> case op of
      Src.Source area srcTarget (Src.Var "++") -> do
        buildApp env target area (Src.Source area srcTarget (Src.Var "assoc")) [argL, argR]

      Src.Source area srcTarget (Src.Var "<|>") -> do
        buildApp env target area (Src.Source area srcTarget (Src.Var "alt")) [argL, argR]

      Src.Source area srcTarget (Src.Var "<>") -> do
        buildApp env target area (Src.Source area srcTarget (Src.Var "mappend")) [argL, argR]

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

    Src.ArrayAccess arr field -> do
      arr'   <- canonicalize env target arr
      field' <- canonicalize env target field
      return $ Can.Canonical area (Can.ArrayAccess arr' field')

    Src.AbsWithMultilineBody params body ->
      processAbs env target area params (expandPatternAssignments body)

    Src.Abs params body ->
      processAbs env target area params (expandPatternAssignments body)

    Src.Return exp ->
      canonicalize env target exp

    Src.Assignment "__EQ__" exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Assignment "==" exp')

    Src.Assignment name exp -> do
      declaredNs <- getDeclaredNameStrings
      when (name `Set.member` declaredNs) $
        pushNameAccess name
      pushNameDeclaration (Env.envExpPosition env) name
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Assignment name exp')

    -- PatternAssignment should be pre-expanded by expandPatternAssignments in all
    -- body/top-level contexts. This fallback handles any remaining cases.
    Src.PatternAssignment pat rhs -> do
      validateIrrefutablePattern env pat
      let expanded = expandPatternAssignments [Src.Source area sourceTarget e]
      exps' <- mapM (canonicalize env target) expanded
      case exps' of
        [single] -> return single
        multiple -> return $ Can.Canonical area (Can.Do multiple)

    Src.Mutate lhs exp -> do
      exp' <- canonicalize env target exp
      lhs' <- canonicalize env target lhs
      validateLhs env lhs' lhs'
      return $ Can.Canonical area (Can.Mutate lhs' exp')

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

      let fieldNames = Maybe.mapMaybe Can.getFieldName fields'
      let uniqFieldNames = Set.toList $ Set.fromList fieldNames
      let fieldDiff = fieldNames List.\\ uniqFieldNames
      unless (null fieldDiff) $
        throwError $ CompilationError (RecordDuplicateFields fieldDiff) (Context (Env.envCurrentPath env) area)


      unless (Can.hasSpread fields') $ do
        pushRecordToDerive fieldNames
      return $ Can.Canonical area (Can.Record fields')

    Src.If cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.While cond body -> do
      cond'   <- canonicalize env target cond
      body'  <- canonicalize env target body
      return $ Can.Canonical area (Can.While cond' body')

    Src.Ternary cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.MaybeDefault maybeExp defaultExp -> do
      maybeExp'   <- canonicalize env target maybeExp
      defaultExp' <- canonicalize env target defaultExp
      let vName = "__maybeDefault__"
          justBranch = Can.Canonical area $
            Can.Is (Can.Canonical area $ Can.PCon "Just" [Can.Canonical area $ Can.PVar vName])
                   (Can.Canonical area $ Can.Var vName)
          defaultBranch = Can.Canonical area $
            Can.Is (Can.Canonical area Can.PAny) defaultExp'
      return $ Can.Canonical area (Can.Where maybeExp' [justBranch, defaultBranch])

    Src.OptionalAccess maybeExp field -> do
      maybeExp' <- canonicalize env target maybeExp
      field'    <- canonicalize env target field
      let vName = "__optChain__"
          accessExp = Can.Canonical area $ Can.Access (Can.Canonical area $ Can.Var vName) field'
          justResult = Can.Canonical area $ Can.App (Can.Canonical area $ Can.Var "Just") accessExp True
          justBranch = Can.Canonical area $
            Can.Is (Can.Canonical area $ Can.PCon "Just" [Can.Canonical area $ Can.PVar vName]) justResult
          nothingBranch = Can.Canonical area $
            Can.Is (Can.Canonical area Can.PAny) (Can.Canonical area $ Can.Var "Nothing")
      return $ Can.Canonical area (Can.Where maybeExp' [justBranch, nothingBranch])

    Src.Do exps -> do
      -- TODO: merge the logic with the one in processAbs
      allAccessesBeforeAbs    <- getAllAccesses
      allDeclaredBeforeAbs    <- getAllDeclaredNames
      declaredNsBefore        <- getDeclaredNameStrings

      resetNameAccesses

      exps' <- canonicalizeDoExps (expandPatternAssignments exps)


      allAccessesInAbs   <- getAllAccesses
      nameAccessesInAbs  <- getAllNameAccesses
      namesDeclaredInAbs <- getAllDeclaredNames

      setAccesses (allAccessesInAbs <> allAccessesBeforeAbs)
      setDeclaredNames (namesDeclaredInAbs <> allDeclaredBeforeAbs)

      let localDecls   = map (\(e, i) -> (Src.getLocalOrNotExportedAssignmentName e, i + Env.envExpPosition env)) (zip exps [0..])
      let localDecls'  = map (\(Just x, i) -> (x, i)) $ filter (Maybe.isJust . fst) localDecls
      let localDecls'' = filter (\(Src.Source _ _ n, _) -> n `Set.notMember` declaredNsBefore) localDecls'
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

      unusedDecls'' <-
        if null unusedDecls' then
          return unusedDecls'
        else do
          return $ filter (not . (allJS =~) . (\(Src.Source _ _ n) -> n)) unusedDecls'

      forM_ unusedDecls'' $ \(Src.Source area' _ n) -> do
        pushWarning $ CompilationWarning (UnusedDeclaration n) (Context (Env.envCurrentPath env) area')

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
        [Src.ParamName (Src.Source emptyArea sourceTarget parameterName)]
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
      return $ Can.Canonical area (Can.App
        (Can.Canonical area (Access (Can.Canonical area (Can.Var "__BUILTINS__")) (Can.Canonical area (Can.Var ".dictFromList"))))
        (Can.Canonical area (Can.ListConstructor items')) True)

    Src.Extern typing name originalName -> do
      scheme  <- typingToScheme env typing
      return $ Can.Canonical area (Can.Extern scheme name originalName)

    Src.TypedHole ->
      return $ Can.Canonical area Can.TypedHole


validateLhs :: Env.Env -> Can.Exp -> Can.Exp -> CanonicalM ()
validateLhs env initialExp exp = case exp of
  Can.Canonical _ (Can.Var _) ->
    return ()

  Can.Canonical _ (Can.Access rec _) ->
    validateLhs env initialExp rec

  Can.Canonical _ (ArrayAccess arr _) ->
    validateLhs env initialExp arr

  _ ->
    throwError $ CompilationError InvalidLhs (Context (Env.envCurrentPath env) (Can.getArea initialExp))


processAbs :: Env.Env -> Target -> Area -> [Src.Param] -> [Src.Exp] -> CanonicalM Can.Exp
processAbs env target area params body = do
  allAccessesBeforeAbs <- getAllAccesses
  allDeclaredBeforeAbs <- getAllDeclaredNames
  declaredNsBefore     <- getDeclaredNameStrings

  resetNameAccesses
  -- resetNamesDeclared

  abs'               <- buildAbs env target area params body

  allAccessesInAbs   <- getAllAccesses
  nameAccessesInAbs  <- getAllNameAccesses
  namesDeclaredInAbs <- getAllDeclaredNames

  setAccesses (allAccessesInAbs <> allAccessesBeforeAbs)
  setDeclaredNames (namesDeclaredInAbs <> allDeclaredBeforeAbs)

  let localDecls   = map (\(e, i) -> (Src.getLocalOrNotExportedAssignmentName e, i + Env.envExpPosition env)) (zip body [0..])
  let localDecls'  = map (\(Just x, i) -> (x, i)) $ filter (Maybe.isJust . fst) localDecls
  let localDecls'' = filter (\(Src.Source _ _ n, _) -> n `Set.notMember` declaredNsBefore) localDecls'
  -- Only check unused params for ParamName entries; ParamPattern entries use synthetic names
  let nameParams = [ src | Src.ParamName src <- params ]
  let unusedParams = filter (\(Src.Source _ _ n) -> n /= "_" && n `Set.notMember` nameAccessesInAbs) nameParams
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

buildAbs :: Env.Env -> Target -> Area -> [Src.Param] -> [Src.Exp] -> CanonicalM Can.Exp
buildAbs env target area@(Area _ (Loc a l c)) [] body = do
  body' <- mapM (\(e, i) -> canonicalize env { Env.envExpPosition = i + Env.envExpPosition env } target e) (zip body [0..])
  let param = Can.Canonical (Area (Loc (a - 1) l (c - 1)) (Loc (a - 1) l (c - 1))) "_"
  return $ Can.Canonical area (Can.Abs param body')
buildAbs env target area [Src.ParamName (Src.Source area' _ param)] body = do
  body' <- mapM (\(e, i) -> canonicalize env { Env.envExpPosition = i + Env.envExpPosition env } target e) (zip body [0..])
  return $ Can.Canonical area (Can.Abs (Can.Canonical area' param) body')
buildAbs env target area [Src.ParamPattern pat] body = do
  validateIrrefutablePattern env pat
  parameterIndex <- generateParameterIndex
  let parameterName = "__D__" ++ show parameterIndex
      patArea = Src.getArea pat
  pat' <- canonicalize env target pat
  body' <- mapM (\(e, i) -> canonicalize env { Env.envExpPosition = i + Env.envExpPosition env } target e) (zip body [0..])
  let bodyExp = case body' of
        [single] -> single
        multiple -> Can.Canonical area (Can.Do multiple)
  -- Use emptyArea for the synthetic __D__ param so hover doesn't show it
  let whereExp = Can.Canonical area (Can.Where
        (Can.Canonical emptyArea (Can.Var parameterName))
        [Can.Canonical area (Can.Is pat' bodyExp)])
  return $ Can.Canonical area (Can.Abs (Can.Canonical emptyArea parameterName) [whereExp])
buildAbs env target area (Src.ParamName (Src.Source area' _ param):xs) body = do
  next <- buildAbs env target area xs body
  return $ Can.Canonical area (Can.Abs (Can.Canonical area' param) [next])
buildAbs env target area (Src.ParamPattern pat:xs) body = do
  validateIrrefutablePattern env pat
  parameterIndex <- generateParameterIndex
  let parameterName = "__D__" ++ show parameterIndex
  pat' <- canonicalize env target pat
  next <- buildAbs env target area xs body
  -- Use emptyArea for synthetic __D__ param so hover doesn't show it
  let whereExp = Can.Canonical area (Can.Where
        (Can.Canonical emptyArea (Can.Var parameterName))
        [Can.Canonical area (Can.Is pat' next)])
  return $ Can.Canonical area (Can.Abs (Can.Canonical emptyArea parameterName) [whereExp])


-- | Validate that a pattern used in a function parameter is irrefutable.
-- Only tuples, records, variables, and wildcards are allowed.
validateIrrefutablePattern :: Env.Env -> Src.Pattern -> CanonicalM ()
validateIrrefutablePattern env pat = case pat of
  Src.Source _ _ (Src.PTuple pats) ->
    mapM_ (validateIrrefutablePattern env) pats
  Src.Source _ _ (Src.PRecord fields) ->
    mapM_ validateField fields
  Src.Source _ _ (Src.PVar _) ->
    return ()
  Src.Source _ _ Src.PAny ->
    return ()
  Src.Source area _ _ ->
    throwError $ CompilationError RefutablePatternInParameter (Context (Env.envCurrentPath env) area)
  where
    validateField (Src.PatternField _ subPat) = validateIrrefutablePattern env subPat
    validateField (Src.PatternFieldShorthand _) = return ()
    validateField (Src.PatternFieldRest _) = return ()


-- | Collect all binding names from a source pattern.
collectPatternNames :: Src.Pattern -> [String]
collectPatternNames (Src.Source _ _ pat) = case pat of
  Src.PVar name  -> [name]
  Src.PAny       -> []
  Src.PTuple ps  -> concatMap collectPatternNames ps
  Src.PRecord fs -> concatMap collectFieldNames fs
  Src.PSpread p  -> collectPatternNames p
  Src.PCon _ ps  -> concatMap collectPatternNames ps
  Src.PNullaryCon _ -> []
  Src.PList ps   -> concatMap collectPatternNames ps
  _              -> []
  where
    collectFieldNames (Src.PatternField _ subPat) = collectPatternNames subPat
    collectFieldNames (Src.PatternFieldShorthand (Src.Source _ _ name)) = [name]
    collectFieldNames (Src.PatternFieldRest (Src.Source _ _ name)) = [name]


-- | Like collectPatternNames but also returns the area of each name binding.
collectPatternNamesWithArea :: Src.Pattern -> [(String, Area)]
collectPatternNamesWithArea (Src.Source area _ pat) = case pat of
  Src.PVar name  -> [(name, area)]
  Src.PAny       -> []
  Src.PTuple ps  -> concatMap collectPatternNamesWithArea ps
  Src.PRecord fs -> concatMap collectFieldNamesWithArea fs
  Src.PSpread p  -> collectPatternNamesWithArea p
  Src.PCon _ ps  -> concatMap collectPatternNamesWithArea ps
  Src.PNullaryCon _ -> []
  Src.PList ps   -> concatMap collectPatternNamesWithArea ps
  _              -> []
  where
    collectFieldNamesWithArea (Src.PatternField _ subPat) = collectPatternNamesWithArea subPat
    collectFieldNamesWithArea (Src.PatternFieldShorthand (Src.Source a _ name)) = [(name, a)]
    collectFieldNamesWithArea (Src.PatternFieldRest (Src.Source a _ name)) = [(name, a)]


-- | Replace the target name with an internal name in a pattern, wildcard everything else.
-- Uses emptyArea for internal names so the LSP doesn't show them on hover.
replaceTargetNameInPat :: String -> String -> Src.Pattern -> Src.Pattern
replaceTargetNameInPat target replacement (Src.Source area t pat) = Src.Source emptyArea t $ case pat of
  Src.PVar n
    | n == target -> Src.PVar replacement
    | otherwise   -> Src.PAny
  Src.PTuple ps   -> Src.PTuple (map (replaceTargetNameInPat target replacement) ps)
  Src.PRecord fs  -> Src.PRecord (map replaceField fs)
  Src.PSpread p   -> Src.PSpread (replaceTargetNameInPat target replacement p)
  Src.PList ps    -> Src.PList (map (replaceTargetNameInPat target replacement) ps)
  Src.PCon n ps   -> Src.PCon n (map (replaceTargetNameInPat target replacement) ps)
  other           -> other
  where
    replaceField field = case field of
      Src.PatternField n subPat -> Src.PatternField n (replaceTargetNameInPat target replacement subPat)
      Src.PatternFieldShorthand (Src.Source _ tt name)
        | name == target -> Src.PatternField (Src.Source emptyArea tt name) (Src.Source emptyArea tt (Src.PVar replacement))
        | otherwise      -> Src.PatternField (Src.Source emptyArea tt name) (Src.Source emptyArea tt Src.PAny)
      Src.PatternFieldRest (Src.Source _ tt name)
        | name == target -> Src.PatternFieldRest (Src.Source emptyArea tt replacement)
        | otherwise      -> Src.PatternFieldRest (Src.Source emptyArea tt "_")


-- | Expand PatternAssignment nodes in a list of expressions into multiple
-- regular assignments. Used both at top level and inside Do/body blocks.
expandPatternAssignments :: [Src.Exp] -> [Src.Exp]
expandPatternAssignments = concatMap expand
  where
    expand (Src.Source area target (Src.PatternAssignment pat rhs)) =
      let tmpName  = "__PA__" ++ show (hashArea area)
          patArea  = Src.getArea pat
          tmpVar   = Src.Source emptyArea target (Src.Var tmpName)
          tmpAssign = Src.Source emptyArea target (Src.Assignment tmpName rhs)
          namesWithAreas = collectPatternNamesWithArea pat
          mkBinding (n, nameArea) =
            let internalName = "__PA_" ++ n ++ "__"
                extractPat = replaceTargetNameInPat n internalName pat
                -- Use nameArea for internal var — sanitizeName strips the __PA_ prefix in hover
                internalVar = Src.Source nameArea target (Src.Var internalName)
                -- Use nameArea for Where/Is so type errors point to the binding's location
                -- and hover works correctly on the pattern variables.
                whereExp = Src.Source nameArea target (Src.Where tmpVar [Src.Source nameArea target (Src.Is extractPat internalVar)])
            -- Use the pattern variable's own area for the binding assignment
            in  Src.Source nameArea target (Src.Assignment n whereExp)
      in  tmpAssign : map mkBinding namesWithAreas
    expand exp = [exp]

    hashArea (Area (Loc a _ _) (Loc b _ _)) = abs (a * 31 + b)


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

  let args'' = take (length args' - canDrop) args'
      wrapperPlaceholderParams' = take (length wrapperPlaceholderParams - canDrop) wrapperPlaceholderParams

  if null args'' then
    canonicalize env target f
  else if null wrapperPlaceholderParams' then
    buildApp' env target (length args'') (length args'') area f args''
  else
    let app = Src.Source area (Src.getSourceTarget f) (Src.App f args'')
    in  buildAbs env target area (map Src.ParamName wrapperPlaceholderParams') [app]


buildApp' :: Env.Env -> Target -> Int -> Int -> Area -> Src.Exp -> [Src.Exp] -> CanonicalM Can.Exp
buildApp' env target total nth area f args =
  buildApp'Rev env target total nth area f (reverse args)

buildApp'Rev :: Env.Env -> Target -> Int -> Int -> Area -> Src.Exp -> [Src.Exp] -> CanonicalM Can.Exp
buildApp'Rev env target total nth area f@Src.Source{} args = case args of
  [arg] -> do
    arg' <- canonicalize env target arg
    f'   <- canonicalize env target f
    return $ Can.Canonical area (Can.App f' arg' (total == nth))

  (arg@(Src.Source area' _ _) : rest) -> do
    arg'   <- canonicalize env target arg
    subApp <- buildApp'Rev env target total (nth - 1) area f rest
    return $ Can.Canonical (mergeAreas area area') (Can.App subApp arg' (total == nth))

  [] ->
    error "buildApp'Rev: empty args list"


canonicalizeJsxTag :: Env.Env -> Target -> Src.Exp -> CanonicalM Can.Exp
canonicalizeJsxTag env target exp = case exp of
  Src.Source area _ (Src.JsxTag name props children) -> do
    pushNameAccess name
    let Area (Loc _ l c) (Loc _ _ _) = area
    let tagFnArea = Area (Loc 0 l c) (Loc 0 l (c + length name + 2))
    let tagFnVar = Can.Canonical tagFnArea (Can.Var name)

    children' <- mapM canonicalizeJsxChild children
    propFields <- mapM
      (\(Src.Source a _ prop) -> case prop of
        Src.JsxProp name' propExp -> do
          arg <- canonicalize env target propExp
          return $ Can.Canonical a (Can.Field (name', arg))
        Src.JsxSpreadProp propExp -> do
          arg <- canonicalize env target propExp
          return $ Can.Canonical a (Can.FieldSpread arg)
      )
      props
    let propArea     = composeArea (length name) area propFields
    let childrenArea = composeArea 0 area children'

    -- Children are included as a `children` field in the props record
    let childrenExp  = Can.Canonical childrenArea (Can.ListConstructor children')
    let childrenField = Can.Canonical childrenArea (Can.Field ("children", childrenExp))
    let allPropFields = propFields ++ [childrenField]
    let props'       = Can.Canonical propArea (Can.JsxRecord allPropFields)

    return $ Can.Canonical area (Can.App tagFnVar props' True)
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

    Src.PNum num -> return $ Can.Canonical area (Can.PNum num)

    Src.PStr str -> return $ Can.Canonical area (Can.PStr str)

    Src.PChar char -> do
      let char' = head $ fst $ last $ charParser char
      return $ Can.Canonical area (Can.PChar char')

    Src.PBool boo -> return $ Can.Canonical area (Can.PBool boo)

    Src.PRecord pats -> do
      let (fields, restName) = extractPatternFields pats
      -- Validate: only one rest pattern allowed
      when (Maybe.isJust restName && length (filter isPatternFieldRest pats) > 1) $
        throwError $ CompilationError (RecordDuplicateRestPattern) (Context (Env.envCurrentPath env) area)
      pats' <- mapM (canonicalize env target) fields
      restName' <- case restName of
        Just (Src.Source _ _ name) -> return $ Just name
        Nothing -> return Nothing
      return $ Can.Canonical area (Can.PRecord pats' restName')

    Src.PList pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PList pats')

    Src.PTuple pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PTuple pats')

    Src.PSpread pat -> do
      pat' <- canonicalize env target pat
      return $ Can.Canonical area (Can.PSpread pat')


extractPatternFields :: [Src.PatternField] -> (Map.Map Src.Name Src.Pattern, Maybe (Src.Source Src.Name))
extractPatternFields pats = case pats of
  (Src.PatternField (Src.Source _ _ fieldName) pat : ps) ->
    let (fields, rest) = extractPatternFields ps
    in (Map.insert fieldName pat fields, rest)

  (Src.PatternFieldShorthand (Src.Source area sourceTarget fieldName) : ps) ->
    let (fields, rest) = extractPatternFields ps
    in (Map.insert fieldName (Src.Source area sourceTarget (Src.PVar fieldName)) fields, rest)

  (Src.PatternFieldRest restSource : ps) ->
    let (fields, _) = extractPatternFields ps
    in (fields, Just restSource)

  [] ->
    (mempty, Nothing)

isPatternFieldRest :: Src.PatternField -> Bool
isPatternFieldRest (Src.PatternFieldRest _) = True
isPatternFieldRest _ = False


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
