{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Infer.ExhaustivePatterns where



{- The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}

-- import qualified Data.List as List
import qualified Data.Map as Map
-- import qualified Data.Maybe as Maybe
-- import qualified Data.Name as Name
-- import qualified Data.NonEmptyList as NE

import qualified AST.Solved as Slv
import           Infer.Infer
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import           Infer.ADTInfo
-- import qualified Data.Index as Index
-- import qualified Elm.ModuleName as ModuleName
-- import qualified Elm.String as ES
-- import qualified Reporting.Annotation as A



-- PATTERN

data Pattern
  = Anything
  | Literal Literal
  | Ctor ADTInfo String [Pattern]


data Literal
  = Chr Char
  | Str String
  | Num String
  deriving (Eq)



-- CREATE SIMPLIFIED PATTERNS


simplify :: Slv.Pattern -> Infer Pattern
simplify (Slv.Untyped _ _)       = undefined
simplify (Slv.Typed _ _ pattern) = case pattern of
  Slv.PAny ->
    return Anything

  Slv.PVar _ ->
    return Anything

  Slv.PRecord _ ->
    return Anything

  -- TODO: Add this pattern
  -- Can.PUnit ->
  --   Ctor unit unitName []

  -- Slv.PTuple pats ->
  --   Ctor pair pairName [ simplify a, simplify b ]

  -- Slv.PTuple a b (Just c) ->
  --   Ctor triple tripleName [ simplify a, simplify b, simplify c ]

  Slv.PCon name args -> do
    args' <- mapM simplify args
    -- TODO: replace that undefined by fetching the ADTInfo
    return $ Ctor undefined name args'

  -- Slv.PList entries ->
  --   foldr cons nil entries

  -- Slv.PList hd tl ->
  --   cons hd (simplify tl)

  -- Can.PAlias subPattern _ ->
  --     simplify subPattern

  Slv.PNum int ->
    return $ Literal (Num int)

  Slv.PStr str ->
    return $ Literal (Str str)

  Slv.PChar chr ->
    return $ Literal (Chr chr)

  _ ->
    undefined

  -- Can.PBool bool ->
  --   Ctor union (if bool then Name.true else Name.false) []


-- cons :: Slv.Pattern -> Pattern -> Infer Pattern
-- cons hd tl =
--   Ctor list consName [ simplify hd, tl ]


-- {-# NOINLINE nil #-}
-- nil :: Pattern
-- nil =
--   Ctor list nilName []



-- -- BUILT-IN UNIONS


-- {-# NOINLINE unit #-}
-- unit :: Can.Union
-- unit =
--   let
--     ctor =
--       Can.Ctor unitName Index.first 0 []
--   in
--   Can.Union [] [ ctor ] 1 Can.Normal


-- {-# NOINLINE pair #-}
-- pair :: Can.Union
-- pair =
--   let
--     ctor =
--       Can.Ctor pairName Index.first 2 [Can.TVar "a", Can.TVar "b"]
--   in
--   Can.Union ["a","b"] [ ctor ] 1 Can.Normal


-- {-# NOINLINE triple #-}
-- triple :: Can.Union
-- triple =
--   let
--     ctor =
--       Can.Ctor tripleName Index.first 3 [Can.TVar "a", Can.TVar "b", Can.TVar "c"]
--   in
--   Can.Union ["a","b","c"] [ ctor ] 1 Can.Normal


-- {-# NOINLINE list #-}
-- list :: Slv.Constructor
-- list =
--   let
--     nilCtor =
--       Slv.Ctor nilName Index.first 0 []

--     consCtor =
--       Can.Ctor consName Index.second 2
--         [ Can.TVar "a"
--         , Can.TType ModuleName.list Name.list [Can.TVar "a"]
--         ]
--   in
--   Can.Union ["a"] [ nilCtor, consCtor ] 2 Can.Normal


-- {-# NOINLINE unitName #-}
-- unitName :: Name.Name
-- unitName = "#0"


-- {-# NOINLINE pairName #-}
-- pairName :: Name.Name
-- pairName = "#2"


-- {-# NOINLINE tripleName #-}
-- tripleName :: Name.Name
-- tripleName = "#3"


-- {-# NOINLINE consName #-}
-- consName :: Name.Name
-- consName = "::"


{-# NOINLINE nilName #-}
nilName :: String
nilName = "[]"



-- ERROR


data Error
  = Incomplete Context [Pattern]
  | Redundant Int


data Context
  = BadArg
  | BadDestruct
  | BadCase



-- -- CHECK


-- check :: Can.Module -> Either (NE.List Error) ()
-- check (Can.Module _ _ _ decls _ _ _ _) =
--   case checkDecls decls [] of
--     [] ->
--       Right ()

--     e:es ->
--       Left (NE.List e es)



-- -- CHECK DECLS


-- checkDecls :: Can.Decls -> [Error] -> [Error]
-- checkDecls decls errors =
--   case decls of
--     Can.Declare def subDecls ->
--       checkDef def $ checkDecls subDecls errors

--     Can.DeclareRec def defs subDecls ->
--       checkDef def (foldr checkDef (checkDecls subDecls errors) defs)

--     Can.SaveTheEnvironment ->
--       errors



-- -- CHECK DEFS


-- checkDef :: Can.Def -> [Error] -> [Error]
-- checkDef def errors =
--   case def of
--     Can.Def _ args body ->
--       foldr checkArg (checkExpr body errors) args

--     Can.TypedDef _ _ args body _ ->
--       foldr checkTypedArg (checkExpr body errors) args


-- checkArg :: Can.Pattern -> [Error] -> [Error]
-- checkArg pattern@(A.At region _) errors =
--   checkPatterns region BadArg [pattern] errors


-- checkTypedArg :: (Can.Pattern, tipe) -> [Error] -> [Error]
-- checkTypedArg (pattern@(A.At region _), _) errors =
--   checkPatterns region BadArg [pattern] errors



-- -- CHECK EXPRESSIONS


-- checkExpr :: Can.Expr -> [Error] -> [Error]
-- checkExpr (A.At region expression) errors =
--   case expression of
--     Can.VarLocal _ ->
--       errors

--     Can.VarTopLevel _ _ ->
--       errors

--     Can.VarKernel _ _ ->
--       errors

--     Can.VarForeign _ _ _ ->
--       errors

--     Can.VarCtor _ _ _ _ _ ->
--       errors

--     Can.VarDebug _ _ _ ->
--       errors

--     Can.VarOperator _ _ _ _ ->
--       errors

--     Can.Chr _ ->
--       errors

--     Can.Str _ ->
--       errors

--     Can.Int _ ->
--       errors

--     Can.Float _ ->
--       errors

--     Can.List entries ->
--       foldr checkExpr errors entries

--     Can.Negate expr ->
--       checkExpr expr errors

--     Can.Binop _ _ _ _ left right ->
--       checkExpr left $
--         checkExpr right errors

--     Can.Lambda args body ->
--       foldr checkArg (checkExpr body errors) args

--     Can.Call func args ->
--       checkExpr func $ foldr checkExpr errors args

--     Can.If branches finally ->
--       foldr checkIfBranch (checkExpr finally errors) branches

--     Can.Let def body ->
--       checkDef def $ checkExpr body errors

--     Can.LetRec defs body ->
--       foldr checkDef (checkExpr body errors) defs

--     Can.LetDestruct pattern@(A.At reg _) expr body ->
--       checkPatterns reg BadDestruct [pattern] $
--         checkExpr expr $ checkExpr body errors

--     Can.Case expr branches ->
--       checkExpr expr $ checkCases region branches errors

--     Can.Accessor _ ->
--       errors

--     Can.Access record _ ->
--       checkExpr record errors

--     Can.Update _ record fields ->
--       checkExpr record $ Map.foldr checkField errors fields

--     Can.Record fields ->
--       Map.foldr checkExpr errors fields

--     Can.Unit ->
--       errors

--     Can.Tuple a b maybeC ->
--       checkExpr a $
--         checkExpr b $
--           case maybeC of
--             Nothing ->
--               errors

--             Just c ->
--               checkExpr c errors

--     Can.Shader _ _ ->
--       errors



-- -- CHECK FIELD


-- checkField :: Can.FieldUpdate -> [Error] -> [Error]
-- checkField (Can.FieldUpdate _ expr) errors =
--   checkExpr expr errors



-- -- CHECK IF BRANCH


-- checkIfBranch :: (Can.Expr, Can.Expr) -> [Error] -> [Error]
-- checkIfBranch (condition, branch) errs =
--   checkExpr condition $ checkExpr branch errs



-- -- CHECK CASE EXPRESSION


-- checkCases :: A.Region -> [Can.CaseBranch] -> [Error] -> [Error]
-- checkCases region branches errors =
--   let
--     (patterns, newErrors) =
--       foldr checkCaseBranch ([], errors) branches
--   in
--   checkPatterns region BadCase patterns newErrors


-- checkCaseBranch :: Can.CaseBranch -> ([Can.Pattern], [Error]) -> ([Can.Pattern], [Error])
-- checkCaseBranch (Can.CaseBranch pattern expr) (patterns, errors) =
--   ( pattern:patterns
--   , checkExpr expr errors
--   )



-- -- CHECK PATTERNS


checkPatterns :: Context -> [Slv.Pattern] -> [Error] -> Infer [Error]
checkPatterns context patterns errors = do
  nonRedundantRows <- toNonRedundantRows patterns
  case nonRedundantRows of
    Left err ->
      return $ err : errors

    Right matrix ->
      case isExhaustive matrix 1 of
        [] ->
          return errors

        badPatterns ->
          return $ Incomplete context (map head badPatterns) : errors



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
      let
        ctors = collectCtors matrix
        numSeen = Map.size ctors
      in
      if numSeen == 0 then
        (:) Anything
          <$> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)

      else
        let alts@(ADTInfo numAlts ctorList) = snd (Map.findMin ctors) in
        if numSeen < numAlts then
          (:)
            <$> Maybe.mapMaybe (isMissing alts ctors) ctorList
            <*> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)

        else
          let
            isAltExhaustive (Slv.Untyped _ (Slv.Constructor name params _)) =
              recoverCtor alts name (length params) <$>
              isExhaustive
                (Maybe.mapMaybe (specializeRowByCtor name (length params)) matrix)
                ((length params) + n - 1)
          in
          concatMap isAltExhaustive ctorList


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
toNonRedundantRows :: [Slv.Pattern] -> Infer (Either Error [[Pattern]])
toNonRedundantRows patterns =
  toSimplifiedUsefulRows [] patterns


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toSimplifiedUsefulRows :: [[Pattern]] -> [Slv.Pattern] -> Infer (Either Error [[Pattern]])
toSimplifiedUsefulRows checkedRows uncheckedPatterns =
  case uncheckedPatterns of
    [] ->
      return $ Right checkedRows

    pattern : rest -> do
      simplified <- simplify pattern
      let nextRow = [simplified]
      if isUseful checkedRows nextRow then
        toSimplifiedUsefulRows (nextRow : checkedRows) rest
      else
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
      error $
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

    Ctor _ _ _ : _ ->
      error $
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

    Ctor _ _ _ : _ ->
      Nothing

    Anything : patterns ->
      Just patterns

    Literal _ : _ ->
      Nothing



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
