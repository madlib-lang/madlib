{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Infer.ExhaustivePatterns where



{- The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}
import qualified Data.Map as Map
import qualified AST.Solved as Slv
import           Infer.Infer
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import           Infer.ADTInfo
import           Infer.Env
import qualified Rock
import           Driver.Query
import           Explain.Location
import           Infer.Type
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Text.Show.Pretty (ppShow)
import           Error.Warning
import           Error.Context
import qualified Data.Set as Set


-- PATTERN

data Pattern
  = Anything
  | Literal Literal
  | Ctor ADTInfo String [Pattern]
  | Record (Map.Map String Pattern)
  deriving(Eq, Show)


data Literal
  = Chr Char
  | Str String
  | Num String
  deriving (Eq, Show)



-- CREATE SIMPLIFIED PATTERNS


simplify :: Slv.AST -> Env -> Slv.Pattern -> Infer Pattern
simplify _ _ (Slv.Untyped _ _)           = undefined
simplify ast env (Slv.Typed _ _ pattern) = case pattern of
  Slv.PAny ->
    return Anything

  Slv.PVar _ ->
    return Anything

  Slv.PRecord fields -> do
    fields' <- mapM (simplify ast env) fields
    return $ Record fields'

  -- TODO: Add this pattern
  -- Can.PUnit ->
  --   Ctor unit unitName []

  Slv.PTuple args -> do
    args' <- mapM (simplify ast env) args
    let name = "(" <> replicate (length args - 1) ',' <> ")"
        ctor = Slv.Untyped emptyArea (Slv.Constructor name (replicate (length args) (Slv.Untyped emptyArea (Slv.TRSingle "a"))) (getTupleCtor (length args)))
    return $ Ctor (ADTInfo 1 [ctor]) name args'

  Slv.PCon name args -> do
    args' <- mapM (simplify ast env) args
    td    <- findTypeDeclByConstructorName ast name
    let ctors = Slv.unsafeGetADTConstructors (Maybe.fromMaybe undefined td)
    return $ Ctor (ADTInfo (length ctors) ctors) name args'

  Slv.PList lis -> do
    let nil     = Ctor (ADTInfo 2 [conCtor, nilCtor]) "__Nil__" []

    case Slv.getSpreadPattern lis of
      Just _ -> do
        buildConsPattern lis

      Nothing ->
        foldM
          (\tl hd -> do
              hd' <- simplify ast env hd
              return $ Ctor (ADTInfo 2 [conCtor, nilCtor]) "__Cons__" [tl, hd']
          )
          nil
          lis
    where
      conCtor = Slv.Untyped emptyArea (Slv.Constructor "__Cons__" [Slv.Untyped emptyArea (Slv.TRSingle "a")] tList)
      nilCtor = Slv.Untyped emptyArea (Slv.Constructor "__Nil__" [] tList)
      buildConsPattern :: [Slv.Pattern] -> Infer Pattern
      buildConsPattern patterns = case patterns of
        [pat] ->
          simplify ast env pat

        pat : pats -> do
          pat'  <- simplify ast env pat
          pats' <- buildConsPattern pats
          return $ Ctor (ADTInfo 2 [conCtor, nilCtor]) "__Cons__" [pats', pat']

  Slv.PSpread pattern ->
    simplify ast env pattern

  Slv.PNum int ->
    return $ Literal (Num int)

  Slv.PStr str ->
    return $ Literal (Str str)

  Slv.PChar chr ->
    return $ Literal (Chr chr)

  Slv.PBool bool -> do
    let trueCtor  = Slv.Untyped emptyArea (Slv.Constructor "true" [] tBool)
        falseCtor = Slv.Untyped emptyArea (Slv.Constructor "false" [] tBool)
    return $ Ctor (ADTInfo 2 [trueCtor, falseCtor]) (if bool == "true" then "true" else "false") []

  _ ->
    return Anything



{-# NOINLINE nilName #-}
nilName :: String
nilName = "[]"



-- ERROR


data Error
  = Incomplete [Pattern]
  | Redundant Int
  deriving(Eq, Show)



-- CHECK


check :: Env -> Slv.AST -> Infer ()
check env ast@Slv.AST { Slv.aexps } = do
  checkExps ast env aexps



-- CHECK DECLS


checkExps :: Slv.AST -> Env -> [Slv.Exp] -> Infer ()
checkExps ast env exps = do
  case exps of
    exp : next -> do
      checkExps ast env next
      checkExp ast env exp

    [] ->
      return ()



-- CHECK EXPRESSIONS


checkExp :: Slv.AST -> Env -> Slv.Exp -> Infer ()
checkExp ast env (Slv.Typed _ area expression) =
  case expression of
    Slv.Assignment _ exp ->
      checkExp ast env exp

    Slv.ListConstructor lis ->
      foldM
        (\_ li -> case li of
            Slv.Typed _ _ (Slv.ListItem e) ->
              checkExp ast env e

            Slv.Typed _ _ (Slv.ListSpread e) ->
              checkExp ast env e
        )
        ()
        lis

    Slv.Where e cases -> do
      checkCases ast env area cases
      checkExp ast env e

    _ ->
      return ()


    -- Can.VarLocal _ ->
    --   errors

    -- Can.VarTopLevel _ _ ->
    --   errors

    -- Can.VarKernel _ _ ->
    --   errors

    -- Can.VarForeign _ _ _ ->
    --   errors

    -- Can.VarCtor _ _ _ _ _ ->
    --   errors

    -- Can.VarDebug _ _ _ ->
    --   errors

    -- Can.VarOperator _ _ _ _ ->
    --   errors

    -- Can.Chr _ ->
    --   errors

    -- Can.Str _ ->
    --   errors

    -- Can.Int _ ->
    --   errors

    -- Can.Float _ ->
    --   errors

    -- Can.List entries ->
    --   foldr checkExp errors entries

    -- Can.Negate expr ->
    --   checkExp expr errors

    -- Can.Binop _ _ _ _ left right ->
    --   checkExp left $
    --     checkExp right errors

    -- Can.Lambda args body ->
    --   foldr checkArg (checkExp body errors) args

    -- Can.Call func args ->
    --   checkExp func $ foldr checkExp errors args

    -- Can.If branches finally ->
    --   foldr checkIfBranch (checkExp finally errors) branches

    -- Can.Let def body ->
    --   checkDef def $ checkExp body errors

    -- Can.LetRec defs body ->
    --   foldr checkDef (checkExp body errors) defs

    -- Can.LetDestruct pattern@(A.At reg _) expr body ->
    --   checkPatterns reg BadDestruct [pattern] $
    --     checkExp expr $ checkExp body errors

    -- Can.Case expr branches ->
    --   checkExp expr $ checkCases region branches errors

    -- Can.Accessor _ ->
    --   errors

    -- Can.Access record _ ->
    --   checkExp record errors

    -- Can.Update _ record fields ->
    --   checkExp record $ Map.foldr checkField errors fields

    -- Can.Record fields ->
    --   Map.foldr checkExp errors fields

    -- Can.Unit ->
    --   errors

    -- Can.Tuple a b maybeC ->
    --   checkExp a $
    --     checkExp b $
    --       case maybeC of
    --         Nothing ->
    --           errors

    --         Just c ->
    --           checkExp c errors

    -- Can.Shader _ _ ->
    --   errors



-- CHECK CASE EXPRESSION


checkCases :: Slv.AST -> Env -> Area -> [Slv.Is] -> Infer ()
checkCases ast env area cases = do
  patterns <- foldM (checkCaseBranch ast env) [] cases
  checkPatterns ast env area patterns


checkCaseBranch :: Slv.AST -> Env -> [Slv.Pattern] -> Slv.Is -> Infer [Slv.Pattern]
checkCaseBranch ast env patterns (Slv.Typed _ _ (Slv.Is pattern exp)) = do
  checkExp ast env exp
  return $ pattern : patterns



-- -- CHECK PATTERNS


checkPatterns :: Slv.AST -> Env -> Area -> [Slv.Pattern] -> Infer ()
checkPatterns ast env area patterns = do
  nonRedundantRows <- toNonRedundantRows ast env patterns
  case nonRedundantRows of
    Left _ ->
      return ()

    Right matrix ->
      case isExhaustive matrix 1 of
        [] ->
          return ()

        badPatterns -> do
          pushWarning (CompilationWarning (IncompletePattern (map showPattern $ concat badPatterns)) (Context (envCurrentPath env) area))


showPattern :: Pattern -> String
showPattern pattern = case pattern of
  Anything ->
    "_"

  Literal lit ->
    case lit of
      Chr c ->
        show c

      Str s ->
        show s

      Num s ->
        s

  Ctor _ name args ->
    if name == "__Cons__" then
      "[" <> List.intercalate ", " (List.replicate (getConsArgCount pattern) "_") <> "]"
    else if name == "__Nil__" then
      "[]"
    else if name `List.elem` [ "(,)"
                             , "(,,)"
                             , "(,,,)"
                             , "(,,,,)"
                             , "(,,,,,)"
                             , "(,,,,,,)"
                             , "(,,,,,,,)"
                             , "(,,,,,,,,)"
                             , "(,,,,,,,,,)"
                             ] then
      "#[" <> showArgs args <> "]"
    else if null args then
      name
    else
      name <> "(" <> showArgs args <> ")"
    where
      showArgs as = List.intercalate ", " (showPattern <$> as)

  Record fields ->
    "{ " <> List.intercalate ", " (map (\(name, pat) -> name <> ": " <> showPattern pat) (Map.toList fields)) <> " }"


getConsArgCount :: Pattern -> Int
getConsArgCount pattern = case pattern of
  Ctor _ "__Cons__" args ->
    1 + getConsArgCount (head args)

  _ ->
    0


-- EXHAUSTIVE PATTERNS


-- INVARIANTS:
--
--   The initial rows "matrix" are all of length 1
--   The initial count of items per row "n" is also 1
--   The resulting rows are examples of missing patterns
--
isExhaustive :: [[Pattern]] -> Int -> [[Pattern]]
isExhaustive matrix n =
  case matrix of
    [] ->
      [replicate n Anything]

    _ ->
      if n == 0 then
        []
      else
      let ctors   = collectCtors matrix
          numSeen = Map.size ctors
      in  if numSeen == 0 then
            let maybeBaseRecord = extractRecordPatterns matrix
            in  case maybeBaseRecord of
              Nothing ->
                (:) Anything
                  <$> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)

              Just baseRecord ->
                let fieldNames = Map.keys baseRecord
                    isAltExhaustive fieldName =
                      isExhaustive
                        (Maybe.mapMaybe (specializeRowByRecordField fieldName) matrix)
                        n
                in  concatMap isAltExhaustive fieldNames
          else
            let alts@(ADTInfo numAlts ctorList) = snd (Map.findMin ctors)
            in  if numSeen < numAlts then
                  (:)
                    <$> Maybe.mapMaybe (isMissing alts ctors) ctorList
                    <*> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)
                else
                  let isAltExhaustive (Slv.Untyped _ (Slv.Constructor name params _)) =
                        recoverCtor alts name (length params) <$>
                        isExhaustive
                          (Maybe.mapMaybe (specializeRowByCtor name (length params)) matrix)
                          (length params + n - 1)
                  in  concatMap isAltExhaustive ctorList


isMissing :: ADTInfo -> Map.Map String a -> Slv.Constructor -> Maybe Pattern
isMissing union ctors (Slv.Untyped _ (Slv.Constructor name params _)) =
  if Map.member name ctors then
    Nothing
  else
    Just (Ctor union name (replicate (length params) Anything))
isMissing _ _ _ = undefined


recoverCtor :: ADTInfo -> String -> Int -> [Pattern] -> [Pattern]
recoverCtor union name arity patterns =
  let
    (args, rest) =
      splitAt arity patterns
  in
  Ctor union name args : rest



-- -- REDUNDANT PATTERNS


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toNonRedundantRows :: Slv.AST -> Env -> [Slv.Pattern] -> Infer (Either Error [[Pattern]])
toNonRedundantRows ast env patterns =
  toSimplifiedUsefulRows ast env [] [] patterns


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toSimplifiedUsefulRows :: Slv.AST -> Env -> [[Pattern]] -> [Slv.Pattern] -> [Slv.Pattern] -> Infer (Either Error [[Pattern]])
toSimplifiedUsefulRows ast env checkedRows checkedPatterns uncheckedPatterns =
  case uncheckedPatterns of
    [] ->
      return $ Right checkedRows

    pattern : rest -> do
      simplified <- simplify ast env pattern
      let nextRow = [simplified]
      if isUseful checkedRows nextRow then
        toSimplifiedUsefulRows ast env (nextRow : checkedRows) (pattern : checkedPatterns) rest
      else do
        -- pushWarning $ CompilationWarning RedundantPattern (Context (envCurrentPath env) (Slv.getArea pattern))
        mapM_
          (pushWarning . CompilationWarning RedundantPattern . Context (envCurrentPath env) . Slv.getArea)
          (pattern : checkedPatterns)
        return $ Left (Redundant (length checkedRows + 1))



-- Check if a new row "vector" is useful given previous rows "matrix"
isUseful :: [[Pattern]] -> [Pattern] -> Bool
isUseful matrix vector =
  case matrix of
    [] ->
      -- No rows are the same as the new vector! The vector is useful!
      True

    _ ->
      case vector of
        [] ->
          -- There is nothing left in the new vector, but we still have
          -- rows that match the same things. This is not a useful vector!
          False

        firstPattern : patterns ->
          case firstPattern of
            Ctor _ name args ->
              -- keep checking rows that start with this Ctor or Anything
              isUseful
                (Maybe.mapMaybe (specializeRowByCtor name (length args)) matrix)
                (args ++ patterns)

            Record recordNamedPatterns ->
              let recordBaseMap = collectRecordFieldsWithAnyPattern matrix
              in  isUseful
                    (Maybe.mapMaybe (specializeRowByRecord recordBaseMap) matrix)
                    (Map.elems recordNamedPatterns ++ patterns)

            Anything ->
              -- check if all alts appear in matrix
              case isComplete matrix of
                No ->
                  -- This Anything is useful because some Ctors are missing.
                  -- But what if a previous row has an Anything?
                  -- If so, this one is not useful.
                  isUseful (Maybe.mapMaybe specializeRowByAnything matrix) patterns

                Yes alts ->
                  -- All Ctors are covered, so this Anything is not needed for any
                  -- of those. But what if some of those Ctors have subpatterns
                  -- that make them less general? If so, this actually is useful!
                  let
                    isUsefulAlt (Slv.Untyped _ (Slv.Constructor name params _)) =
                      isUseful
                        (Maybe.mapMaybe (specializeRowByCtor name (length params)) matrix)
                        (replicate (length params) Anything ++ patterns)
                  in
                    any isUsefulAlt alts

            Literal literal ->
              -- keep checking rows that start with this Literal or Anything
              isUseful
                (Maybe.mapMaybe (specializeRowByLiteral literal) matrix)
                patterns


-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByCtor :: String -> Int -> [Pattern] -> Maybe [Pattern]
specializeRowByCtor ctorName arity row =
  case row of
    Ctor _ name args : patterns ->
      if name == ctorName then
        Just (args ++ patterns)
      else
        Nothing

    Anything : patterns ->
      Just (replicate arity Anything ++ patterns)

    Record _ : _ ->
      Nothing

    Literal _ : _ ->
      error
        "Compiler bug! After type checking, constructors and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      Just []


-- INVARIANT: (length row == N) ==> (length result == N-1)
specializeRowByLiteral :: Literal -> [Pattern] -> Maybe [Pattern]
specializeRowByLiteral literal row =
  case row of
    Literal lit : patterns ->
      if lit == literal then
        Just patterns
      else
        Nothing

    Anything : patterns ->
      Just patterns

    Ctor {} : _ ->
      error
        "Compiler bug! After type checking, constructors and literals\
        \ should never align in pattern match exhaustiveness checks."

    Record _ : _ ->
      error
        "Compiler bug! After type checking, records and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      error "Compiler error! Empty matrices should not get specialized."


-- INVARIANT: (length row == N) ==> (length result == N-1)
specializeRowByAnything :: [Pattern] -> Maybe [Pattern]
specializeRowByAnything row =
  case row of
    [] ->
      Nothing

    Ctor {} : _ ->
      Nothing

    Record _ : _ ->
      Nothing

    Anything : patterns ->
      Just patterns

    Literal _ : _ ->
      Nothing


-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByRecord :: Map.Map String Pattern -> [Pattern] -> Maybe [Pattern]
specializeRowByRecord baseMap row =
  case row of
    Ctor{} : _ ->
      Nothing

    Record namedPatterns : patterns ->
      let specializedMap = Map.union namedPatterns baseMap
      in  Just (Map.elems specializedMap ++ patterns)

    Anything : patterns ->
      Just (Map.elems baseMap ++ patterns)

    Literal _ : _ ->
      error
        "Compiler bug! After type checking, records and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      error "Compiler error! Empty matrices should not get specialized."

-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByRecordField :: String -> [Pattern] -> Maybe [Pattern]
specializeRowByRecordField fieldName row =
  case row of
    Ctor{} : _ ->
      Nothing

    Anything : patterns ->
      Just (Anything : patterns)

    Record namedPatterns : patterns ->
      case Map.lookup fieldName namedPatterns of
        Just pattern ->
          Just (pattern : patterns)

        Nothing ->
          Nothing

    Literal _ : _ ->
      error
        "Compiler bug! After type checking, constructors and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      error "Compiler error! Empty matrices should not get specialized."



-- ALL CONSTRUCTORS ARE PRESENT?


data Complete
  = Yes [Slv.Constructor]
  | No


isComplete :: [[Pattern]] -> Complete
isComplete matrix =
  let
    ctors = collectCtors matrix
    numSeen = Map.size ctors
  in
    if numSeen == 0 then
      No
    else
      let (ADTInfo numAlts alts) = snd (Map.findMin ctors) in
      if numSeen == numAlts then Yes alts else No



-- COLLECT CTORS


collectCtors :: [[Pattern]] -> Map.Map String ADTInfo
collectCtors matrix =
  List.foldl' collectCtorsHelp Map.empty matrix


collectCtorsHelp :: Map.Map String ADTInfo -> [Pattern] -> Map.Map String ADTInfo
collectCtorsHelp ctors row =
  case row of
    Ctor union name _ : _ ->
      Map.insert name union ctors

    _ ->
      ctors


-- COLLECT RECORD FIELDS
extractRecordPatterns :: [[Pattern]] -> Maybe (Map.Map String Pattern)
extractRecordPatterns matrix =
  if containsRecord matrix then
    Just $ collectRecordFieldsWithAnyPattern matrix
  else
    Nothing

containsRecord :: [[Pattern]] -> Bool
containsRecord matrix =
  case matrix of
    [] ->
      False

    (Record _ : _) : _ ->
      True

    _ : rest ->
      containsRecord rest

collectRecordFieldsWithAnyPattern :: [[Pattern]] -> Map.Map String Pattern
collectRecordFieldsWithAnyPattern matrix =
  let fieldNames = List.foldl' collectRecordFields Set.empty matrix
   in Set.foldl' (\fields name -> Map.insert name Anything fields) Map.empty fieldNames

collectRecordFields :: Set.Set String -> [Pattern] -> Set.Set String
collectRecordFields nameCollection row =
  case row of
    Record namedPatterns : _ ->
      Set.union
        (Set.fromList (Map.keys namedPatterns))
        nameCollection

    _ ->
      nameCollection


findTypeDeclByConstructorName :: Rock.MonadFetch Query m => Slv.AST -> String -> m (Maybe Slv.TypeDecl)
findTypeDeclByConstructorName Slv.AST { Slv.atypedecls, Slv.aimports } ctorName = do
  let searchInForeignModule _ = do
        let (importPath, realCtorName) =
              if "." `List.isInfixOf` ctorName then
                let namespace = List.takeWhile (/= '.') ctorName
                    ctorName' = tail $ List.dropWhile (/= '.') ctorName
                    foundImport =
                      Maybe.fromMaybe (error ("namespace is: " <> namespace)) $ List.find
                        (\case
                          Slv.Untyped _ (Slv.DefaultImport (Slv.Untyped _ name) _ _) ->
                            name == namespace

                          _ -> False
                        )
                        aimports
                in  (Slv.getImportAbsolutePath foundImport, ctorName')
              else
                let foundImport =
                      Maybe.fromMaybe undefined $ List.find
                        (\case
                          Slv.Untyped _ (Slv.NamedImport names _ _) ->
                            any ((== ctorName) . Slv.getValue) names

                          _ -> False
                        )
                        aimports
                in  (Slv.getImportAbsolutePath foundImport, ctorName)
        Rock.fetch $ ForeignTypeDeclaration importPath realCtorName
  let foundTypeDecl = List.find
        (\case
            Slv.Untyped _ Slv.ADT { Slv.adtconstructors } ->
              any ((== ctorName) . Slv.getConstructorName) adtconstructors

            _ ->
              False
        )
        atypedecls
  case foundTypeDecl of
    Just found ->
      return $ Just found

    Nothing ->
      searchInForeignModule ()
