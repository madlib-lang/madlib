{-# LANGUAGE NamedFieldPuns #-}
module Canonicalize.Derived where
import Canonicalize.CanonicalM
import AST.Canonical
import Infer.Type
import Explain.Location
import qualified Data.Map as Map
import Debug.Trace
import Text.Show.Pretty
import Data.Maybe (mapMaybe)


-- utility to create an empty canonical node
ec :: a -> Canonical a
ec = Canonical emptyArea


generateCtorParamPatternNames :: Char -> [Typing] -> [String]
generateCtorParamPatternNames prefix typings =
  (prefix:) . show . fst <$> zip [0..] typings


buildConstructorIs :: Constructor -> Is
buildConstructorIs ctor = case ctor of
  Canonical _ (Constructor name typings _) ->
    let aVars = generateCtorParamPatternNames 'a' typings
        bVars = generateCtorParamPatternNames 'b' typings
        conditions =
          if null typings then
            ec $ LBool "true"
          else
            foldr
              (\(aVar, bVar) previousCondition ->
                  ec $ App
                    (ec $ App
                      (ec $ Var "&&")
                      (ec $ App
                        (ec $ App
                          (ec $ Var "==")
                          (ec $ Var aVar)
                          False
                        )
                        (ec $ Var bVar)
                        True
                      )
                      False
                    )
                    previousCondition
                    True
              )
              (ec $ LBool "true")
              (zip aVars bVars)
    in
      ec $ Is (ec $ PTuple [
        ec $ PCon name (ec . PVar <$> aVars),
        ec $ PCon name (ec . PVar <$> bVars)
      ]) conditions


isFnTyping :: Typing -> Bool
isFnTyping typing = case typing of
  Canonical _ (TRArr _ _) ->
    True

  _ ->
    False

areConstructorsValid :: [Constructor] -> Bool
areConstructorsValid ctors =
  let paramTypings = concat $ (\(Canonical _ (Constructor _ typings _)) -> typings) <$> ctors
  in  not (any isFnTyping paramTypings)

deriveEqInstance :: ToDerive -> Maybe Instance
deriveEqInstance toDerive = case toDerive of
  TypeDeclToDerive (Canonical _ ADT { adtname, adtparams, adtconstructors, adtType }) ->
    let varsInType = mapMaybe (`searchVarInType` adtType) adtparams
    in  if any ((/= Star) . kind) varsInType || not (areConstructorsValid adtconstructors) then
          Nothing
        else
          let instPreds  =
                (\varInType ->
                    IsIn "Eq" [varInType] Nothing
                ) <$> varsInType
              inst =
                ec (
                  Instance
                  "Eq"
                  instPreds
                  (IsIn "Eq" [adtType] Nothing)
                  (
                    Map.singleton
                    "=="
                    (
                      ec (Assignment "==" (ec $ Abs (ec "a") [ec $ Abs (ec "b") [
                        ec $ Where (ec (TupleConstructor [ec $ Var "a", ec $ Var "b"]))
                          (
                            (buildConstructorIs <$> adtconstructors)
                              ++  [
                                    -- if no previous pattern matches then the two values are not equal
                                    ec $ Is (ec PAny) (ec $ LBool "false")
                                  ]
                          )
                      ]]))
                    )
                  )
                )
          in  Just inst

  _ ->
    undefined
