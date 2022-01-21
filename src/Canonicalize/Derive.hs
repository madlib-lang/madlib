{-# LANGUAGE NamedFieldPuns #-}
module Canonicalize.Derive where
import Canonicalize.CanonicalM
import AST.Canonical
import Infer.Type
import Explain.Location
import qualified Data.Map as Map
import Debug.Trace
import Text.Show.Pretty
import Data.Maybe (mapMaybe)
import Data.ByteString.Char8 (intersperse)
import qualified Data.List as List


-- utility to create an empty canonical node
ec :: a -> Canonical a
ec = Canonical emptyArea


chars :: [String]
chars = (:"") <$> ['a'..]


generateCtorParamPatternNames :: Char -> [Typing] -> [String]
generateCtorParamPatternNames prefix typings =
  (prefix:) . show . fst <$> zip [0..] typings


buildConstructorIsForEq :: Constructor -> Is
buildConstructorIsForEq ctor = case ctor of
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


buildFieldConditions :: [String] -> Exp
buildFieldConditions =
  foldr
    (\fieldName previousCondition ->
      ec $ App
        (ec $ App
          (ec $ Var "&&")
          (ec $ App
            (ec $ App
              (ec $ Var "==")
              (ec $ Access (ec $ Var "__$a__") (ec $ Var ('.':fieldName)))
              False
            )
            (ec $ Access (ec $ Var "__$b__") (ec $ Var ('.':fieldName)))
            True
          )
          False
        )
        previousCondition
        True
    )
    (ec $ LBool "true")


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
                      ec (Assignment "==" (ec $ Abs (ec "__$a__") [ec $ Abs (ec "__$b__") [
                        ec $ Where (ec (TupleConstructor [ec $ Var "__$a__", ec $ Var "__$b__"]))
                          (
                            (buildConstructorIsForEq <$> adtconstructors)
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

  RecordToDerive fieldNames ->
    let fieldNamesWithVars = zip fieldNames chars
        fields             = TVar . (`TV` Star) <$> Map.fromList fieldNamesWithVars
        recordType         = TRecord fields Nothing
        instPreds          = (\var -> IsIn "Eq" [var] Nothing) <$> Map.elems fields
    in  Just $ ec (Instance "Eq" instPreds (IsIn "Eq" [recordType] Nothing) (
          Map.singleton
          "=="
          (
            ec (Assignment "==" (ec $ Abs (ec "__$a__") [ec $ Abs (ec "__$b__") [
              buildFieldConditions fieldNames
            ]]))
          )
        ))


  _ ->
    undefined


inspectFields :: [String] -> Exp
inspectFields fieldNames =
  let fields =
        (\fieldName ->
            let fieldNameStr = ec $ LStr (fieldName <> ": ")
                fieldValue   = (ec $ Access (ec $ Var "__$a__") (ec $ Var ('.':fieldName)))
                inspectedFieldValue = ec $ App (ec $ Var "inspect") fieldValue True
            in  ec $ App (ec $ App (ec $ Var "++") fieldNameStr False) inspectedFieldValue True
        ) <$> fieldNames
      commaSeparated = List.intersperse (ec $ LStr ", ") fields
  in  ec $ TemplateString ([ec $ LStr "{ "] ++ commaSeparated ++ [ec $ LStr " }"])

buildConstructorIsForInspect :: Constructor -> Is
buildConstructorIsForInspect ctor = case ctor of
  Canonical _ (Constructor name typings _) ->
    let vars = generateCtorParamPatternNames 'a' typings
        inspected =
          if null typings then
            ec $ LStr ("\"" <> name <> "\"")
          else
            let constructorNameLStr = ec $ LStr ("" <> name <> "(")
                closingParenthesis  = ec $ LStr ")"
                inspectedValues     = (\var -> ec $ App (ec $ Var "inspect") (ec $ Var var) True) <$> vars
                commaSeparated      = List.intersperse (ec $ LStr ", ") inspectedValues
            in  ec $ TemplateString ([constructorNameLStr] ++ commaSeparated ++ [closingParenthesis])
    in
      ec $ Is (ec $ PCon name (ec . PVar <$> vars)) inspected


deriveInspectInstance :: ToDerive -> Maybe Instance
deriveInspectInstance toDerive = case toDerive of
  TypeDeclToDerive (Canonical _ ADT { adtname, adtparams, adtconstructors, adtType }) ->
    let varsInType = mapMaybe (`searchVarInType` adtType) adtparams
        instPreds  =
                (\varInType ->
                    IsIn "Inspect" [varInType] Nothing
                ) <$> varsInType
    in  if any ((/= Star) . kind) varsInType || not (areConstructorsValid adtconstructors) then
          let inst =
                ec (
                  Instance
                  "Inspect"
                  instPreds
                  (IsIn "Inspect" [adtType] Nothing)
                  (
                    Map.singleton
                    "inspect"
                    (
                      ec (Assignment "inspect" (ec $ Abs (ec "__$a__") [
                        ec $ LStr ("\"[Opaque ADT: " <> adtname <> "]\"")
                      ]))
                    )
                  )
                )
            in Just inst
        else
          let inst =
                ec (
                  Instance
                  "Inspect"
                  instPreds
                  (IsIn "Inspect" [adtType] Nothing)
                  (
                    Map.singleton
                    "inspect"
                    (
                      ec (Assignment "inspect" (ec $ Abs (ec "__$a__") [
                        ec $ Where (ec $ Var "__$a__")
                          (
                            (buildConstructorIsForInspect <$> adtconstructors)
                              ++  [
                                    -- if no previous pattern matches then the two values are not equal
                                    ec $ Is (ec PAny) (ec $ LStr "\"Unknown\"")
                                  ]
                          )
                      ]))
                    )
                  )
                )
          in  Just inst

  RecordToDerive fieldNames ->
    let fieldNamesWithVars = zip fieldNames chars
        fields             = TVar . (`TV` Star) <$> Map.fromList fieldNamesWithVars
        recordType         = TRecord fields Nothing
        instPreds          = (\var -> IsIn "Inspect" [var] Nothing) <$> Map.elems fields
    in  Just $ ec (Instance "Inspect" instPreds (IsIn "Inspect" [recordType] Nothing) (
          Map.singleton
          "inspect"
          (
            ec (Assignment "inspect" (ec $ Abs (ec "__$a__") [
              inspectFields fieldNames
            ]))
          )
        ))


  _ ->
    undefined