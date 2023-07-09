{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.Derive where

import           Canonicalize.CanonicalM
import           Canonicalize.InstanceToDerive
import           AST.Canonical
import qualified AST.Source                            as Src
import           Infer.Type
import           Explain.Location
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set
import           Data.Maybe (mapMaybe)
import           Data.ByteString.Char8 (intersperse)
import qualified Data.List                              as List
import qualified Data.Maybe                             as Maybe
import qualified Canonicalize.Env                       as Can
import Utils.Hash
import Utils.Record (generateRecordPredsAndType)
import qualified Rock
import qualified Driver.Query as Query


searchTypeInConstructor :: Id -> Type -> Maybe Type
searchTypeInConstructor id t = case t of
  TVar (TV n _) ->
    if n == id then Just t else Nothing

  TCon _ _ ->
    Nothing

  TApp l r ->
    case (l, r) of
      (TVar (TV n _), _) | n == id ->
        Just t

      _ ->
        case (searchTypeInConstructor id l, searchTypeInConstructor id r) of
          (Just x, _     ) -> Just x
          (_     , Just x) -> Just x
          _                -> Nothing

  _ ->
    Nothing


-- utility to create an empty canonical node
ec :: a -> Canonical a
ec = Canonical emptyArea


chars :: [String]
chars = ("f_"++) . show <$> [0..]


generateCtorParamPatternNames :: Char -> [Typing] -> [String]
generateCtorParamPatternNames prefix typings =
  (prefix:) . show . fst <$> zip [0..] typings


buildConstructorIsForEq :: Constructor -> Is
buildConstructorIsForEq ctor = case ctor of
  Canonical _ (Constructor name typings _ _) ->
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


deriveEqInstance :: FilePath -> InstanceToDerive -> Maybe Instance
deriveEqInstance astPath toDerive = case toDerive of
  TypeDeclToDerive (Canonical _ ADT { adtparams, adtconstructors, adtType }) ->
    let constructorTypes = getCtorType <$> adtconstructors
        varsInType  = Set.toList $ Set.fromList $ concat $ (\t -> mapMaybe (`searchTypeInConstructor` t) adtparams) <$> constructorTypes
        instPreds  =
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
    let (instPreds, recordType) = generateRecordPredsAndType astPath "Eq" (Set.toList fieldNames)
    in  Just $ ec (Instance "Eq" instPreds (IsIn "Eq" [recordType] Nothing) (
          Map.singleton
          "=="
          (
            ec (Assignment "==" (ec $ Abs (ec "__$a__") [ec $ Abs (ec "__$b__") [
              buildFieldConditions (Set.toList fieldNames)
            ]]))
          )
        ))


  _ ->
    undefined


showFields :: [String] -> Exp
showFields fieldNames =
  let fields =
        (\fieldName ->
            let fieldNameStr = ec $ LStr ("\""<> fieldName <> ": \"")
                fieldValue   = ec $ Access (ec $ Var "__$a__") (ec $ Var ('.':fieldName))
                showedFieldValue = ec $ App (ec $ Var "show") fieldValue True
            in  ec $ App (ec $ App (ec $ Var "++") fieldNameStr False) showedFieldValue True
        ) <$> fieldNames
      commaSeparated = List.intersperse (ec $ LStr "\", \"") fields
  in  ec $ TemplateString ([ec $ LStr "\"{ \""] ++ commaSeparated ++ [ec $ LStr "\" }\""])


buildConstructorIsForShow :: Constructor -> Is
buildConstructorIsForShow ctor = case ctor of
  Canonical _ (Constructor name typings _ _) ->
    let vars = generateCtorParamPatternNames 'a' typings
        showed =
          if null typings then
            ec $ LStr ("\"" <> name <> "\"")
          else
            let constructorNameLStr = ec $ LStr ("\"" <> name <> "(\"")
                closingParenthesis  = ec $ LStr "\")\""
                showedValues     = (\var -> ec $ App (ec $ Var "show") (ec $ Var var) True) <$> vars
                commaSeparated      = List.intersperse (ec $ LStr "\", \"") showedValues
            in  ec $ TemplateString ([constructorNameLStr] ++ commaSeparated ++ [closingParenthesis])
    in
      ec $ Is (ec $ PCon name (ec . PVar <$> vars)) showed


deriveShowInstance :: FilePath -> InstanceToDerive -> Maybe Instance
deriveShowInstance astPath toDerive = case toDerive of
  TypeDeclToDerive (Canonical _ ADT { adtparams, adtconstructors, adtType }) ->
    let constructorTypes = getCtorType <$> adtconstructors
        varsInType  = Set.toList $ Set.fromList $ concat $ (\t -> mapMaybe (`searchTypeInConstructor` t) adtparams) <$> constructorTypes
        instPreds   = (\varInType -> IsIn "Show" [varInType] Nothing) <$> varsInType
        inst =
          ec (
            Instance
            "Show"
            instPreds
            (IsIn "Show" [adtType] Nothing)
            (
              Map.singleton
              "show"
              (
                ec (Assignment "show" (ec $ Abs (ec "__$a__") [
                  ec $ Where (ec $ Var "__$a__")
                    (
                      (buildConstructorIsForShow <$> adtconstructors)
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
    let (instPreds, recordType) = generateRecordPredsAndType astPath "Show" (Set.toList fieldNames)
    in  Just $ ec (Instance "Show" instPreds (IsIn "Show" [recordType] Nothing) (
          Map.singleton
          "show"
          (
            ec (Assignment "show" (ec $ Abs (ec "__$a__") [
              showFields (Set.toList fieldNames)
            ]))
          )
        ))


  _ ->
    undefined



builtinsAccess :: String -> Exp
builtinsAccess name =
  ec $ Access (ec $ Var "__BUILTINS__") (ec $ Var $ '.' : name)


buildArgsComparisons :: Int -> [(String, String)] -> Exp
buildArgsComparisons index args = case args of
  [(argLeft, argRight)] ->
    ec (App (ec (App (ec (Var "compare")) (ec (Var argLeft)) False)) (ec (Var argRight)) True)

  (argLeft, argRight) : nextArgs ->
    let resultName = "__r__" <> show index
    in  ec (Do
          [ ec (Assignment resultName (ec (App (ec (App (ec (Var "compare")) (ec (Var argLeft)) False)) (ec (Var argRight)) True)))
          , ec
              (If
                (ec (App (ec (App (ec (Var "==")) (ec (Var resultName)) False)) (builtinsAccess "EQ") True))
                (buildArgsComparisons (index + 1) nextArgs)
                (ec (Var resultName)))
          ])

  [] ->
    builtinsAccess "EQ"


buildMoreBranches :: [Constructor] -> [Is]
buildMoreBranches ctors = case ctors of
  [] ->
    [ec (Is (ec PAny) (builtinsAccess "LT"))]

  Canonical _ (Constructor name typings _ _) : next ->
    let current = ec (Is (ec $ PCon name (ec PAny <$ typings)) (builtinsAccess "GT"))
    in  current : buildMoreBranches next


buildConstructorIsForCompare :: [Constructor] -> Constructor -> [Is]
buildConstructorIsForCompare allConstructors ctor = case ctor of
  Canonical _ (Constructor name typings _ _) ->
    let aVars = generateCtorParamPatternNames 'a' typings
        bVars = generateCtorParamPatternNames 'b' typings
        firstBranchBody = buildArgsComparisons 0 (List.zip aVars bVars)
        sameCtorsBranch = ec $ Is (ec $ PTuple [ec $ PCon name (ec . PVar <$> aVars), ec $ PCon name (ec . PVar <$> bVars)]) firstBranchBody
        constructorsBefore = filter ((== GT) . compareConstructors ctor) allConstructors
        differentCtorsBranchBody =
          if null constructorsBefore then
            builtinsAccess "LT"
          else
            ec (Where (ec $ Var "__other__") (buildMoreBranches constructorsBefore))
        otherPVar =
          if null constructorsBefore then
            ec PAny
          else
            ec (PVar "__other__")
        differentCtorsBranch = ec $ Is (ec $ PTuple [ec $ PCon name (ec PAny <$ aVars), otherPVar]) differentCtorsBranchBody
    in  [sameCtorsBranch, differentCtorsBranch]



buildFieldComparisons :: Int -> [String] -> Exp
buildFieldComparisons index fieldNames = case fieldNames of
  [fieldName] ->
    ec (App (ec (App (ec (Var "compare")) (ec (Access (ec (Var "__$a__")) (ec (Var ('.' : fieldName))))) False)) (ec (Access (ec (Var "__$b__")) (ec (Var ('.' : fieldName))))) True)

  fieldName : nextFields ->
    let resultName = "__r__" <> show index
    in  ec (Do
          [ ec (Assignment resultName (ec (App (ec (App (ec (Var "compare"))
                          (ec (Access (ec (Var "__$a__")) (ec (Var ('.' : fieldName))))) False))
                          (ec (Access (ec (Var "__$b__")) (ec (Var ('.' : fieldName)))))
                          True)))
          , ec
              (If
                (ec (App (ec (App (ec (Var "==")) (ec (Var resultName)) False)) (builtinsAccess "EQ") True))
                (buildFieldComparisons (index + 1) nextFields)
                (ec (Var resultName)))
          ])

  [] ->
    builtinsAccess "EQ"



compareConstructors :: Constructor -> Constructor -> Ordering
compareConstructors a b = compare (getConstructorName a) (getConstructorName b) 


deriveComparableADTInstance :: TypeDecl -> Maybe Instance
deriveComparableADTInstance adt = case adt of
  Canonical _ ADT { adtparams, adtconstructors, adtType } ->
    let constructorTypes = getCtorType <$> adtconstructors
        varsInType  = Set.toList $ Set.fromList $ concat $ (\t -> mapMaybe (`searchTypeInConstructor` t) adtparams) <$> constructorTypes
        instPreds   = (\varInType -> IsIn "Comparable" [varInType] Nothing) <$> varsInType
        -- TODO: make generic
        sortedConstructors = List.sortBy compareConstructors adtconstructors
        branches = sortedConstructors >>= buildConstructorIsForCompare sortedConstructors
        inst =
          ec (
            Instance
            "Comparable"
            instPreds
            (IsIn "Comparable" [adtType] Nothing)
            (
              Map.singleton
              "compare"
              (
                ec (Assignment "compare" (ec (Abs (ec "__$a__") [ ec (Abs (ec "__$b__") [
                  ec (Where
                    (ec (TupleConstructor [ec (Var "__$a__"), ec (Var "__$b__")]))
                    branches
                  )])
                ])))
              )
            )
          )
    in  Just inst

  _ ->
    -- TODO: implement
    Nothing


deriveInstances :: Can.Env -> [TypeDecl] -> [Src.Derived] -> CanonicalM [Instance]
deriveInstances env localTypeDecls derived = case derived of
  Src.Source _ _ (Src.DerivedADT adtName) : next ->
    case Map.lookup adtName (Can.envTypeDecls env) of
      Just t -> do
        let path = getTConPath t
        maybeADT <-
          if path == Can.envCurrentPath env then
            return $ List.find ((== adtName) . getTypeDeclName) localTypeDecls
          else
            Rock.fetch $ Query.ForeignCanTypeDeclaration path adtName

        case maybeADT of
          Just adt -> do
            let derivedInstance = deriveComparableADTInstance adt
            nextInstances <- deriveInstances env localTypeDecls next

            case derivedInstance of
              Just inst ->
                return $ inst : nextInstances

          Nothing ->
            deriveInstances env localTypeDecls next

      Nothing ->
        deriveInstances env localTypeDecls next

  Src.Source _ _ (Src.DerivedRecord fieldNames) : next -> do
    let (instPreds, recordType) = generateRecordPredsAndType (Can.envCurrentPath env) "Comparable" fieldNames
        derivedInstance = ec (Instance "Comparable" instPreds (IsIn "Comparable" [recordType] Nothing) (
            Map.singleton
            "compare"
            (
              ec (Assignment "==" (ec $ Abs (ec "__$a__") [ec $ Abs (ec "__$b__") [
                buildFieldComparisons 0 fieldNames
              ]]))
            )
          ))

    nextInstances <- deriveInstances env localTypeDecls next
    return $ derivedInstance : nextInstances

  [] ->
    return []
