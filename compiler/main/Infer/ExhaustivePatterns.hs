{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import Debug.Trace


-- PATTERN

data Pattern
  = Anything
  | Literal Literal
  | Ctor ADTInfo String [Pattern]
  deriving(Eq)

instance Show Pattern where
  show pattern = case pattern of
    Anything ->
      "Anything"

    Literal lit ->
      "Literal " <> show lit

    Ctor _ name pats ->
      "Ctor " <> name <> show pats

data Literal
  = Chr Char
  | Str String
  | Num String
  deriving (Eq, Show)



-- CREATE SIMPLIFIED PATTERNS

simplify :: Slv.AST -> Env -> Slv.Pattern -> Infer Pattern
simplify _ _ (Slv.Untyped _ _)           = undefined
simplify ast env (Slv.Typed (_ :=> t) _ pattern) = case pattern of
  Slv.PAny ->
    return Anything

  Slv.PVar _ ->
    return Anything

  Slv.PRecord fields -> do
    fields' <- mapM (simplify ast env) fields
    let allFields = case t of
          TRecord fieldTypes _ ->
            Map.union
              fields'
              (Map.map (const Anything) fieldTypes)

          _ ->
            fields'
    -- return $ Record allFields

    let pats = Map.elems allFields
        adtInfo = ADTInfo 1 [Slv.Untyped emptyArea $ Slv.Constructor "__RECORD__" (replicate (length pats) (Slv.Untyped emptyArea (Slv.TRSingle "a"))) t]
    return $ Ctor adtInfo "__RECORD__" pats


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
    case td of
      Nothing ->
        -- Most likely a constructor that does not exist, in that case we skip
        -- it by returning Anything as an error has already been emitted for
        -- this.
        return Anything

      Just td' -> do
        let ctors = Slv.unsafeGetADTConstructors td'
        return $ Ctor (ADTInfo (length ctors) ctors) (canonicalizeCtorName name) args'

  Slv.PList lis -> do
    let nil     = Ctor (ADTInfo 2 [conCtor, nilCtor]) "__Nil__" []

    case Slv.getSpreadPattern lis of
      Just _ ->
        buildConsPattern lis
        -- buildConsPattern (reverse lis)

      Nothing ->
        foldM
          (\tl hd -> do
              hd' <- simplify ast env hd
              return $ Ctor (ADTInfo 2 [conCtor, nilCtor]) "__Cons__" [tl, hd']
          )
          nil
          lis
    where
      conCtor = Slv.Untyped emptyArea (Slv.Constructor "__Cons__" [Slv.Untyped emptyArea (Slv.TRSingle "a"), Slv.Untyped emptyArea (Slv.TRSingle "a")] tList)
      nilCtor = Slv.Untyped emptyArea (Slv.Constructor "__Nil__" [] tList)
      buildConsPattern :: [Slv.Pattern] -> Infer Pattern
      buildConsPattern patterns = case patterns of
        [pat] ->
          simplify ast env pat

        pat : pats -> do
          pat'  <- simplify ast env pat
          pats' <- buildConsPattern pats
          return $ Ctor (ADTInfo 2 [conCtor, nilCtor]) "__Cons__" [pats', pat']

        _ ->
          undefined

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



-- ERROR

data Error
  = Incomplete [Pattern]
  | Redundant Int
  deriving(Eq, Show)



-- CHECK

check :: Env -> Slv.AST -> Infer ()
check env ast@Slv.AST { Slv.aexps } = do
  checkExps ast env aexps



-- CHECK Exps

checkExps :: Slv.AST -> Env -> [Slv.Exp] -> Infer ()
checkExps ast env exps = do
  case exps of
    exp : next -> do
      checkExps ast env next
      checkExp ast env exp

    [] ->
      return ()


checkExp :: Slv.AST -> Env -> Slv.Exp -> Infer ()
checkExp _ _ (Slv.Untyped _ _)                 = undefined
checkExp ast env (Slv.Typed _ area expression) =
  case expression of
    Slv.Assignment _ exp ->
      checkExp ast env exp

    Slv.TypedExp exp _ _ ->
      checkExp ast env exp

    Slv.App fn arg _ -> do
      checkExp ast env fn
      checkExp ast env arg

    Slv.Abs _ body ->
      mapM_ (checkExp ast env) body

    Slv.Do body ->
      mapM_ (checkExp ast env) body

    Slv.Var _ _ ->
      return ()

    Slv.If cond truthy falsy -> do
      checkExp ast env cond
      checkExp ast env truthy
      checkExp ast env falsy

    Slv.NameExport _ ->
      return ()

    Slv.Placeholder _ exp ->
      checkExp ast env exp

    Slv.Access rec field -> do
      checkExp ast env rec
      checkExp ast env field

    Slv.Extern {} ->
      return ()

    Slv.TypeExport _ ->
      return ()

    Slv.Export exp ->
      checkExp ast env exp

    Slv.TupleConstructor exps ->
      mapM_ (checkExp ast env) exps

    Slv.ListConstructor lis ->
      foldM
        (\_ li -> case li of
            Slv.Typed _ _ (Slv.ListItem e) ->
              checkExp ast env e

            Slv.Typed _ _ (Slv.ListSpread e) ->
              checkExp ast env e

            Slv.Untyped _ _ ->
              undefined
        )
        ()
        lis

    Slv.Record fields ->
      foldM
        (\_ li -> case li of
            Slv.Typed _ _ (Slv.Field (_, e)) ->
              checkExp ast env e

            Slv.Typed _ _ (Slv.FieldSpread e) ->
              checkExp ast env e

            Slv.Untyped _ _ ->
              undefined
        )
        ()
        fields

    Slv.Where e cases -> do
      checkCases ast env area cases
      checkExp ast env e

    Slv.LNum _ ->
      return ()

    Slv.LFloat _ ->
      return ()

    Slv.LBool _ ->
      return ()

    Slv.LStr _ ->
      return ()

    Slv.LChar _ ->
      return ()

    Slv.LUnit ->
      return ()

    Slv.JSExp _ ->
      return ()

    Slv.TypedHole ->
      return ()

    Slv.TemplateString exps ->
      mapM_ (checkExp ast env) exps



-- CHECK CASE EXPRESSION

checkCases :: Slv.AST -> Env -> Area -> [Slv.Is] -> Infer ()
checkCases ast env area cases = do
  patterns <- foldM (checkCaseBranch ast env) [] cases
  checkPatterns ast env area patterns


checkCaseBranch :: Slv.AST -> Env -> [Slv.Pattern] -> Slv.Is -> Infer [Slv.Pattern]
checkCaseBranch _ _ _ (Slv.Untyped _ _)                               = undefined
checkCaseBranch ast env patterns (Slv.Typed _ _ (Slv.Is pattern exp)) = do
  checkExp ast env exp
  return $ pattern : patterns



-- CHECK PATTERNS

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
          let badPatterns' = map head $ filter (not . null) badPatterns
          unless (null badPatterns') $
            pushWarning (CompilationWarning (IncompletePattern (map showPattern badPatterns')) (Context (envCurrentPath env) area))


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

  Ctor ai name args ->
    if name == "__RECORD__" then
      let (ADTInfo _ [Slv.Untyped _ (Slv.Constructor _ _ (TRecord fields _))]) = ai
          fields' = zip (Map.keys fields) args
      in  "{ " <> List.intercalate ", " (map (\(name, pat) -> name <> ": " <> showPattern pat) fields') <> " }"
    else if name == "__Cons__" then
      case getConsArgs pattern of
        [] ->
          "[]"

        [arg] ->
          "[" <> showPattern arg <> "]"

        realArgs ->
          if last realArgs == Anything then
            "[" <> List.intercalate ", " (map showPattern (init realArgs)) <> ", ..._" <> "]"
          else
            "[" <> List.intercalate ", " (map showPattern realArgs) <> "]"
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


getConsArgs :: Pattern -> [Pattern]
getConsArgs pattern = case pattern of
  Ctor _ "__Cons__" [tail, arg] ->
    arg : getConsArgs tail

  Ctor _ "__Nil__" _ ->
    []

  arg ->
    [arg]


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
              (:) Anything
                <$> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)
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
  let (args, rest) = splitAt arity patterns
  in  Ctor union name args : rest



-- -- REDUNDANT PATTERNS


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toNonRedundantRows :: Slv.AST -> Env -> [Slv.Pattern] -> Infer (Either () [[Pattern]])
toNonRedundantRows ast env patterns =
  toSimplifiedUsefulRows ast env [] [] (reverse patterns)


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toSimplifiedUsefulRows :: Slv.AST -> Env -> [[Pattern]] -> [Slv.Pattern] -> [Slv.Pattern] -> Infer (Either () [[Pattern]])
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
        mapM_
          (pushWarning . CompilationWarning RedundantPattern . Context (envCurrentPath env) . Slv.getArea)
          [pattern]
        toSimplifiedUsefulRows ast env (nextRow : checkedRows) (pattern : checkedPatterns) rest


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

    Literal _ : _ ->
      error
        "Compiler bug! After type checking, constructors and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      error "Compiler error! Empty matrices should not get specialized."


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

    Anything : patterns ->
      Just (Map.elems baseMap ++ patterns)

    Literal _ : _ ->
      error
        "Compiler bug! After type checking, records and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      error "Compiler error! Empty matrices should not get specialized."

-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByRecordField :: Int -> String -> [Pattern] -> Maybe [Pattern]
specializeRowByRecordField fieldCount fieldName row =
  case row of
    Ctor{} : _ ->
      Nothing

    Anything : patterns ->
      -- Just (Anything : patterns)
      Just (replicate fieldCount Anything ++ patterns)

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



findTypeDeclInASTByConstructorName :: Slv.AST -> String -> Maybe Slv.TypeDecl
findTypeDeclInASTByConstructorName Slv.AST { Slv.atypedecls } ctorName =
  List.find
    (\case
        Slv.Untyped _ Slv.ADT { Slv.adtconstructors } ->
          any ((== ctorName) . Slv.getConstructorName) adtconstructors

        _ ->
          False
    )
    atypedecls


canonicalizeCtorName :: String -> String
canonicalizeCtorName ctorName =
  if "." `List.isInfixOf` ctorName then
    tail $ List.dropWhile (/= '.') ctorName
  else
    ctorName


-- TODO: see if we can optimize this
findTypeDeclByConstructorName :: Rock.MonadFetch Query m => Slv.AST -> String -> m (Maybe Slv.TypeDecl)
findTypeDeclByConstructorName ast@Slv.AST { Slv.aimports } ctorName = do
  let searchInForeignModule _ = do
        let (importPath, realCtorName) =
              if "." `List.isInfixOf` ctorName then
                let namespace = List.takeWhile (/= '.') ctorName
                    ctorName' = tail $ List.dropWhile (/= '.') ctorName
                    foundImport =
                      List.find
                        (\case
                          Slv.Untyped _ (Slv.DefaultImport (Slv.Untyped _ name) _ _) ->
                            name == namespace

                          _ -> False
                        )
                        aimports
                in  (Slv.getImportAbsolutePath <$> foundImport, ctorName')
              else
                let foundImport =
                      List.find
                        (\case
                          Slv.Untyped _ (Slv.NamedImport names _ _) ->
                            any ((== ctorName) . Slv.getValue) names

                          _ -> False
                        )
                        aimports
                in  (Slv.getImportAbsolutePath <$> foundImport, ctorName)
        case importPath of
          Nothing ->
            return Nothing

          Just importPath' -> do
            (ast', _) <- Rock.fetch $ SolvedASTWithEnv importPath'
            case findTypeDeclInASTByConstructorName ast' realCtorName of
              Just found ->
                return $ Just found

              Nothing ->
                findTypeDeclByConstructorName ast' realCtorName
  let foundTypeDecl = findTypeDeclInASTByConstructorName ast ctorName
  case foundTypeDecl of
    Just found ->
      return $ Just found

    Nothing ->
      searchInForeignModule ()
