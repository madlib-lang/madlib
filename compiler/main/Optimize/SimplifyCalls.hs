{-# LANGUAGE LambdaCase #-}
module Optimize.SimplifyCalls where
import AST.Core
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Infer.Type as IT

{-
Replaces expressions of the form
  ((x) => x + 1)(5)
with
  5 + 1

It is not very common, but that is mainly what pipe(..)(..) generates when called directly
and creates an extra function.

Because it seems to somehow alter program behavior when generalized, it's currently
limited to pipes, where shorthand, and placeholder args ( $ ).
-}

replaceVarInIsWith :: String -> Exp -> Is -> Is
replaceVarInIsWith n replaceWith is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (replaceVarWith n replaceWith exp))

  _ ->
    is


replaceVarWith :: String -> Exp -> Exp -> Exp
replaceVarWith n replaceWith exp = case exp of
  Typed qt area metadata (Assignment name e) ->
    Typed qt area metadata (Assignment name (replaceVarWith n replaceWith e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (replaceVarWith n replaceWith e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (replaceVarWith n replaceWith fn) (replaceVarWith n replaceWith <$> args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (replaceVarWith n replaceWith <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem (replaceVarWith n replaceWith) <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (replaceVarWith n replaceWith <$> items))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (replaceVarWith n replaceWith rec) (replaceVarWith n replaceWith field))

  Typed qt area metadata (ArrayAccess arr index) ->
    Typed qt area metadata (ArrayAccess (replaceVarWith n replaceWith arr) (replaceVarWith n replaceWith index))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField (replaceVarWith n replaceWith) <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (replaceVarWith n replaceWith cond) (replaceVarWith n replaceWith truthy) (replaceVarWith n replaceWith falsy))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (replaceVarWith n replaceWith cond) (replaceVarWith n replaceWith body))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (replaceVarWith n replaceWith <$> exps))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (replaceVarWith n replaceWith exp) (replaceVarInIsWith n replaceWith <$> iss))

  Typed _ _ _ (Var name _) | name == n ->
    replaceWith

  _ ->
    exp


reduceIsWithPureFns :: Set.Set String -> Is -> Is
reduceIsWithPureFns pureFns is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (reduceWithPureFns pureFns exp))

  _ ->
    is


isEligible :: Exp -> Bool
isEligible exp = case exp of
  Typed _ _ _ (Assignment _ e) ->
    isEligible e

  Typed _ _ _ (Export e) ->
    isEligible e

  Typed _ _ _ (Call fn args) ->
    isEligible fn && all isEligible args

  Typed _ _ _ (Definition _ body) ->
    all isEligible body

  Typed _ _ _ (ListConstructor items) ->
    all
      (\case
        Typed _ _ _ (ListItem e) ->
          isEligible e

        Typed _ _ _ (ListSpread e) ->
          isEligible e

        _ ->
          True
      )
      items

  Typed _ _ _ (TupleConstructor items) ->
    all isEligible items

  Typed _ _ _ (Access rec field) ->
    isEligible rec && isEligible field

  Typed _ _ _ (ArrayAccess arr index) ->
    isEligible arr && isEligible index

  Typed _ _ _ (Record fields) ->
    all
      (\case
        Typed _ _ _ (Field (_, e)) ->
          isEligible e

        Typed _ _ _ (FieldSpread e) ->
          isEligible e

        _ ->
          True
      )
      fields

  Typed _ _ _ (If cond truthy falsy) ->
    isEligible cond && isEligible truthy && isEligible falsy

  Typed _ _ _ (While cond body) ->
    isEligible cond && isEligible body

  Typed _ _ _ (Do exps) ->
    all isEligible exps

  Typed _ _ _ (Where exp iss) ->
    isEligible exp &&
    all
      (\(Typed _ _ _ (Is _ e)) ->
        isEligible e
      )
      iss

  Typed _ _ _ (JSExp _) ->
    False

  _ ->
    True


occurencesOf :: String -> Exp -> Int
occurencesOf name exp = case exp of
  Typed _ _ _ (Assignment _ e) ->
    occurencesOf name e

  Typed _ _ _ (Export e) ->
    occurencesOf name e

  Typed _ _ _ (Call fn args) ->
    occurencesOf name fn + foldr ((+) . occurencesOf name) 0 args

  Typed _ _ _ (Definition _ body) ->
    foldr ((+) . occurencesOf name) 0 body

  Typed _ _ _ (ListConstructor items) ->
    foldr
      ((+) . \case
        Typed _ _ _ (ListItem e) ->
          occurencesOf name e

        Typed _ _ _ (ListSpread e) ->
          occurencesOf name e

        _ ->
          0
      )
      0
      items

  Typed _ _ _ (TupleConstructor items) ->
    foldr ((+) . occurencesOf name) 0 items

  Typed _ _ _ (Record fields) ->
    foldr
      ((+) . \case
        Typed _ _ _ (Field (_, e)) ->
          occurencesOf name e

        Typed _ _ _ (FieldSpread e) ->
          occurencesOf name e

        _ ->
          0
      )
      0
      fields

  Typed _ _ _ (Access rec field) ->
    occurencesOf name rec + occurencesOf name field

  Typed _ _ _ (ArrayAccess arr index) ->
    occurencesOf name arr + occurencesOf name index

  Typed _ _ _ (If cond truthy falsy) ->
    occurencesOf name cond + occurencesOf name truthy + occurencesOf name falsy

  Typed _ _ _ (While cond body) ->
    occurencesOf name cond + occurencesOf name body

  Typed _ _ _ (Do exps) ->
    foldr ((+) . occurencesOf name) 0 exps

  Typed _ _ _ (Where exp iss) ->
    occurencesOf name exp +
    foldr
      ((+) . \(Typed _ _ _ (Is _ e)) ->
        occurencesOf name e
      )
      0
      iss

  Typed _ _ _ (Var n _) ->
    if n == name then
      1
    else
      0

  Typed _ _ _ (JSExp _) ->
    0

  _ ->
    0


isLiteralOrVar :: Exp -> Bool
isLiteralOrVar exp = case exp of
  Typed _ _ _ (Literal _) ->
    True

  Typed _ _ _ (Var _ _) ->
    True

  _ ->
    False

isMapFnName :: String -> Bool
isMapFnName fnName =
  "_map__" `List.isInfixOf` fnName

isRepeatFnName :: String -> Bool
isRepeatFnName fnName =
  "_repeat__" `List.isInfixOf` fnName

isLengthFnName :: String -> Bool
isLengthFnName fnName =
  "_length__" `List.isInfixOf` fnName

isNthFnName :: String -> Bool
isNthFnName fnName =
  "_nth__" `List.isInfixOf` fnName

isPureLibraryFnName :: String -> Bool
isPureLibraryFnName fnName =
  isRepeatFnName fnName || isMapFnName fnName || isLengthFnName fnName || isNthFnName fnName

isPureIntrinsicName :: String -> Bool
isPureIntrinsicName fnName =
  fnName `elem`
    [ "+"
    , "-"
    , "unary-minus"
    , "*"
    , "/"
    , "%"
    , "<<"
    , ">>"
    , ">>>"
    , "|"
    , "&"
    , "^"
    , "~"
    , "=="
    , ">"
    , "<"
    , ">="
    , "<="
    , "&&"
    , "||"
    , "!"
    ]

isPureExpr :: Set.Set String -> Exp -> Bool
isPureExpr allowedVars exp = case exp of
  Typed _ _ _ (Literal _) ->
    True

  Typed _ _ _ (Var name _) ->
    name `Set.member` allowedVars

  Typed _ _ _ (Call (Typed _ _ _ (Var fnName _)) args) ->
    isPureIntrinsicName fnName && all (isPureExpr allowedVars) args

  Typed _ _ _ (Call _ _) ->
    False

  Typed _ _ _ (ListConstructor items) ->
    all (isPureExpr allowedVars . getListItemExp) items

  Typed _ _ _ (TupleConstructor items) ->
    all (isPureExpr allowedVars) items

  Typed _ _ _ (Record fields) ->
    all (isPureExpr allowedVars . getFieldExp) fields

  Typed _ _ _ (Access rec field) ->
    isPureExpr allowedVars rec && isPureExpr allowedVars field

  Typed _ _ _ (ArrayAccess arr index) ->
    isPureExpr allowedVars arr && isPureExpr allowedVars index

  Typed _ _ _ (If cond truthy falsy) ->
    isPureExpr allowedVars cond && isPureExpr allowedVars truthy && isPureExpr allowedVars falsy

  Typed _ _ _ (Where e iss) ->
    isPureExpr allowedVars e && all (\(Typed _ _ _ (Is _ body)) -> isPureExpr allowedVars body) iss

  _ ->
    False

collectPureUnaryTopLevelFns :: [Exp] -> Set.Set String
collectPureUnaryTopLevelFns exps =
  Set.fromList $ foldr collect [] exps
  where
    collect exp found = case exp of
      Typed _ _ _ (Assignment (Typed _ _ _ (Var fnName _)) (Typed _ _ _ (Definition [Typed _ _ _ paramName] [body])))
        | isPureExpr (Set.singleton paramName) body ->
            fnName : found

      Typed _ _ _ (Export inner) ->
        collect inner found

      _ ->
        found

extractRepeatCall :: Exp -> Maybe (Exp, Exp, Exp)
extractRepeatCall exp = case exp of
  Typed _ _ _ (Call repeatFn@(Typed _ _ _ (Var repeatName _)) [item, count])
    | isRepeatFnName repeatName ->
        Just (repeatFn, item, count)

  _ ->
    Nothing

extractIntegerLiteral :: Exp -> Maybe Integer
extractIntegerLiteral exp = case exp of
  Typed _ _ _ (Literal (LNum n)) ->
    let normalized = filter (/= '_') n
    in case reads normalized of
      [(parsed, "")] ->
        Just parsed

      _ ->
        Nothing

  _ ->
    Nothing

mkNumberLiteralLike :: Exp -> Integer -> Exp
mkNumberLiteralLike sample n =
  Typed (getQualType sample) (getArea sample) (getMetadata sample) (Literal (LNum (show n)))

mkEmptyListLike :: Exp -> Exp
mkEmptyListLike sample =
  Typed (getQualType sample) (getArea sample) (getMetadata sample) (ListConstructor [])

mkSingletonListLike :: Exp -> Exp -> Exp
mkSingletonListLike sample item =
  Typed
    (getQualType sample)
    (getArea sample)
    (getMetadata sample)
    (ListConstructor [Typed (getQualType item) (getArea item) (getMetadata item) (ListItem item)])

collectVarsUsed :: Exp -> Set.Set String
collectVarsUsed exp = case exp of
  Typed _ _ _ (Literal _) ->
    Set.empty

  Typed _ _ _ (JSExp _) ->
    Set.empty

  Typed _ _ _ (Var name _) ->
    Set.singleton name

  Typed _ _ _ (Definition params body) ->
    let paramNames = Set.fromList (getValue <$> params)
        usedInBody = Set.unions (collectVarsUsed <$> body)
    in Set.difference usedInBody paramNames

  Typed _ _ _ (Call fn args) ->
    Set.unions (collectVarsUsed fn : (collectVarsUsed <$> args))

  Typed _ _ _ (Access rec field) ->
    Set.union (collectVarsUsed rec) (collectVarsUsed field)

  Typed _ _ _ (ArrayAccess arr index) ->
    Set.union (collectVarsUsed arr) (collectVarsUsed index)

  Typed _ _ _ (Assignment _ rhs) ->
    collectVarsUsed rhs

  Typed _ _ _ (Export e) ->
    collectVarsUsed e

  Typed _ _ _ (NameExport _) ->
    Set.empty

  Typed _ _ _ (ListConstructor items) ->
    Set.unions (collectVarsUsed . getListItemExp <$> items)

  Typed _ _ _ (TupleConstructor items) ->
    Set.unions (collectVarsUsed <$> items)

  Typed _ _ _ (Record fields) ->
    Set.unions (collectVarsUsed . getFieldExp <$> fields)

  Typed _ _ _ (If cond truthy falsy) ->
    Set.unions [collectVarsUsed cond, collectVarsUsed truthy, collectVarsUsed falsy]

  Typed _ _ _ (While cond body) ->
    Set.union (collectVarsUsed cond) (collectVarsUsed body)

  Typed _ _ _ (Do exps) ->
    Set.unions (collectVarsUsed <$> exps)

  Typed _ _ _ (Where e iss) ->
    Set.union (collectVarsUsed e) (Set.unions (collectVarsUsed . getIsExpression <$> iss))

  Typed _ _ _ (Extern _ _ _) ->
    Set.empty

  Typed _ _ _ TypedHole ->
    Set.empty

  _ ->
    Set.empty

isSideEffectFree :: Set.Set String -> Exp -> Bool
isSideEffectFree pureFns exp = case exp of
  Typed _ _ _ (Literal _) ->
    True

  Typed _ _ _ (Var _ _) ->
    True

  Typed _ _ _ (JSExp _) ->
    False

  Typed _ _ _ (Definition _ body) ->
    all (isSideEffectFree pureFns) body

  Typed _ _ _ (Call (Typed _ _ _ (Var fnName isCtor)) args) ->
    (isCtor || isPureIntrinsicName fnName || isPureLibraryFnName fnName || fnName `Set.member` pureFns)
      && all (isSideEffectFree pureFns) args

  Typed _ _ _ (Call _ _) ->
    False

  Typed _ _ _ (Access rec field) ->
    isSideEffectFree pureFns rec && isSideEffectFree pureFns field

  Typed _ _ _ (ArrayAccess arr index) ->
    isSideEffectFree pureFns arr && isSideEffectFree pureFns index

  Typed _ _ _ (Assignment _ rhs) ->
    isSideEffectFree pureFns rhs

  Typed _ _ _ (Export e) ->
    isSideEffectFree pureFns e

  Typed _ _ _ (NameExport _) ->
    True

  Typed _ _ _ (ListConstructor items) ->
    all (isSideEffectFree pureFns . getListItemExp) items

  Typed _ _ _ (TupleConstructor items) ->
    all (isSideEffectFree pureFns) items

  Typed _ _ _ (Record fields) ->
    all (isSideEffectFree pureFns . getFieldExp) fields

  Typed _ _ _ (If cond truthy falsy) ->
    isSideEffectFree pureFns cond
      && isSideEffectFree pureFns truthy
      && isSideEffectFree pureFns falsy

  Typed _ _ _ (While _ _) ->
    False

  Typed _ _ _ (Do exps) ->
    all (isSideEffectFree pureFns) exps

  Typed _ _ _ (Where e iss) ->
    isSideEffectFree pureFns e && all (isSideEffectFree pureFns . getIsExpression) iss

  Typed _ _ _ (Extern _ _ _) ->
    False

  Typed _ _ _ TypedHole ->
    False

  _ ->
    False

eliminateUnusedAssignmentsInBody :: Set.Set String -> [Exp] -> [Exp]
eliminateUnusedAssignmentsInBody pureFns exps =
  snd (foldl step (Set.empty, []) (reverse exps))
  where
    step (needed, acc) exp = case exp of
      Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) rhs) ->
        if name `Set.member` needed || not (isSideEffectFree pureFns rhs) then
          let needed' = Set.union (Set.delete name needed) (collectVarsUsed rhs)
          in (needed', exp : acc)
        else
          (needed, acc)

      _ ->
        let needed' = Set.union needed (collectVarsUsed exp)
        in (needed', exp : acc)

isDirectMapUseOfVar :: Set.Set String -> String -> Exp -> Bool
isDirectMapUseOfVar pureFns varName exp = case exp of
  Typed _ _ _ (Assignment _ (Typed _ _ _ (Call (Typed _ _ _ (Var mapName _)) [Typed _ _ _ (Var fName _), Typed _ _ _ (Var usedVar _)])))
    | isMapFnName mapName
      , fName `Set.member` pureFns
      , usedVar == varName ->
        True

  Typed _ _ _ (Call (Typed _ _ _ (Var mapName _)) [Typed _ _ _ (Var fName _), Typed _ _ _ (Var usedVar _)])
    | isMapFnName mapName
      , fName `Set.member` pureFns
      , usedVar == varName ->
        True

  _ ->
    False

rewriteMapRepeatWithEnv :: Set.Set String -> Map.Map String (Exp, Exp, Exp) -> Exp -> Exp
rewriteMapRepeatWithEnv pureFns knownRepeats exp = case exp of
  Typed qt area metadata (Call lengthFn@(Typed _ _ _ (Var lengthName _)) [listArg])
    | isLengthFnName lengthName ->
        let listArg' = rewriteMapRepeatWithEnv pureFns knownRepeats listArg
            fromRepeat (_, _, count) = rewriteMapRepeatWithEnv pureFns knownRepeats count
        in case extractRepeatCall listArg' of
            Just repeatData ->
              fromRepeat repeatData

            Nothing ->
              case listArg' of
                Typed _ _ _ (Var listName _) ->
                  case Map.lookup listName knownRepeats of
                    Just repeatData ->
                      fromRepeat repeatData

                    Nothing ->
                      Typed qt area metadata (Call lengthFn [listArg'])

                _ ->
                  Typed qt area metadata (Call lengthFn [listArg'])

  Typed qt area metadata (Call nthFn@(Typed _ _ _ (Var nthName _)) [indexArg, listArg])
    | isNthFnName nthName ->
        let indexArg' = rewriteMapRepeatWithEnv pureFns knownRepeats indexArg
            listArg' = rewriteMapRepeatWithEnv pureFns knownRepeats listArg
            fallback = Typed qt area metadata (Call nthFn [indexArg', listArg'])
            foldNthWithRepeat item count =
              case (extractIntegerLiteral indexArg', extractIntegerLiteral count) of
                (Just i, Just c)
                  | i >= 0 && i < c ->
                      let zeroIndex = mkNumberLiteralLike indexArg' 0
                          singleton = mkSingletonListLike listArg' item
                      in Typed qt area metadata (Call nthFn [zeroIndex, singleton])

                  | otherwise ->
                      let oneIndex = mkNumberLiteralLike indexArg' 1
                          emptyList = mkEmptyListLike listArg'
                      in Typed qt area metadata (Call nthFn [oneIndex, emptyList])

                _ ->
                  fallback
        in case extractRepeatCall listArg' of
            Just (_, item, count) ->
              foldNthWithRepeat item count

            Nothing ->
              case listArg' of
                Typed _ _ _ (Var listName _) ->
                  case Map.lookup listName knownRepeats of
                    Just (_, item, count) ->
                      foldNthWithRepeat item count

                    Nothing ->
                      fallback

                _ ->
                  fallback

  Typed qt area metadata (Call mapFn@(Typed _ _ _ (Var mapName _)) [fArg@(Typed _ _ _ (Var fName _)), listArg])
    | isMapFnName mapName
      , fName `Set.member` pureFns ->
        let listArg' = rewriteMapRepeatWithEnv pureFns knownRepeats listArg
            fusedFrom (repeatFn, item, count) =
              let item' = rewriteMapRepeatWithEnv pureFns knownRepeats item
                  count' = rewriteMapRepeatWithEnv pureFns knownRepeats count
                  fAppliedType = [] IT.:=> IT.dropFirstParamType (getType fArg)
                  fApplied = Typed fAppliedType area metadata (Call fArg [item'])
              in Typed qt area metadata (Call repeatFn [fApplied, count'])
        in case extractRepeatCall listArg' of
             Just repeatData ->
               fusedFrom repeatData

             Nothing ->
               case listArg' of
                 Typed _ _ _ (Var listName _) ->
                   case Map.lookup listName knownRepeats of
                     Just repeatData ->
                       fusedFrom repeatData

                     Nothing ->
                       Typed qt area metadata (Call mapFn [fArg, listArg'])

                 _ ->
                   Typed qt area metadata (Call mapFn [fArg, listArg'])

  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (rewriteMapRepeatWithEnv pureFns knownRepeats rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (rewriteMapRepeatWithEnv pureFns knownRepeats e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (rewriteMapRepeatWithEnv pureFns knownRepeats fn) (map (rewriteMapRepeatWithEnv pureFns knownRepeats) args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (map (rewriteMapRepeatWithEnv pureFns knownRepeats) body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem (rewriteMapRepeatWithEnv pureFns knownRepeats) <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (map (rewriteMapRepeatWithEnv pureFns knownRepeats) items))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (rewriteMapRepeatWithEnv pureFns knownRepeats rec) (rewriteMapRepeatWithEnv pureFns knownRepeats field))

  Typed qt area metadata (ArrayAccess arr index) ->
    Typed qt area metadata (ArrayAccess (rewriteMapRepeatWithEnv pureFns knownRepeats arr) (rewriteMapRepeatWithEnv pureFns knownRepeats index))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField (rewriteMapRepeatWithEnv pureFns knownRepeats) <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (rewriteMapRepeatWithEnv pureFns knownRepeats cond) (rewriteMapRepeatWithEnv pureFns knownRepeats truthy) (rewriteMapRepeatWithEnv pureFns knownRepeats falsy))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (rewriteMapRepeatWithEnv pureFns knownRepeats cond) (rewriteMapRepeatWithEnv pureFns knownRepeats body))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (map (rewriteMapRepeatWithEnv pureFns knownRepeats) exps))

  Typed qt area metadata (Where e iss) ->
    Typed qt area metadata (Where (rewriteMapRepeatWithEnv pureFns knownRepeats e) (map (mapIs (rewriteMapRepeatWithEnv pureFns knownRepeats)) iss))

  _ ->
    exp

fuseMapRepeatInBody :: Set.Set String -> [Exp] -> [Exp]
fuseMapRepeatInBody pureFns exps = reverse $ go Map.empty Nothing [] exps
  where
    flushPending pending acc = case pending of
      Nothing ->
        acc

      Just (_, pendingExp) ->
        pendingExp : acc

    go knownRepeats pending acc body = case body of
      exp : rest ->
        let exp' = rewriteMapRepeatWithEnv pureFns knownRepeats exp
        in case exp' of
            Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) rhs)
              | Just repeatData <- extractRepeatCall rhs ->
                  let acc' = flushPending pending acc
                      knownRepeats' = Map.insert n repeatData knownRepeats
                  in go knownRepeats' (Just (n, exp')) acc' rest

            _ ->
              case pending of
                Just (pendingName, _) | isDirectMapUseOfVar pureFns pendingName exp ->
                  go knownRepeats Nothing (exp' : acc) rest

                _ ->
                  let acc' = flushPending pending acc
                  in go knownRepeats Nothing (exp' : acc') rest

      [] ->
        flushPending pending acc

reduceWithPureFns :: Set.Set String -> Exp -> Exp
reduceWithPureFns pureFns exp = case exp of
  Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param@(Typed _ _ _ pName)] [body])) [arg@(Typed _ _ _ (Var aName _))])
    | "__$PH" `List.isPrefixOf` pName ||
      "__P__" `List.isPrefixOf` pName ||
      "__M__" `List.isPrefixOf` pName ||
      "__W__" `List.isPrefixOf` pName ||
      "__$PH" `List.isPrefixOf` aName ||
      "__P__" `List.isPrefixOf` aName ||
      "__M__" `List.isPrefixOf` aName ||
      "__W__" `List.isPrefixOf` aName ->
        if isEligible body && (isLiteralOrVar arg || occurencesOf pName body == 1) then
          reduceWithPureFns pureFns $ replaceVarWith (getValue param) (reduceWithPureFns pureFns arg) (reduceWithPureFns pureFns body)
        else
          Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param] [reduceWithPureFns pureFns body])) [reduceWithPureFns pureFns arg])

  Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param@(Typed _ _ _ pName)] [body])) [arg])
    | "__$PH" `List.isPrefixOf` pName || "__P__" `List.isPrefixOf` pName || "__M__" `List.isPrefixOf` pName || "__W__" `List.isPrefixOf` pName ->
      if isEligible body && (isLiteralOrVar arg || occurencesOf pName body == 1) then
        reduceWithPureFns pureFns $ replaceVarWith (getValue param) (reduceWithPureFns pureFns arg) (reduceWithPureFns pureFns body)
      else
        Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param] [reduceWithPureFns pureFns body])) [reduceWithPureFns pureFns arg])

  Typed qt area metadata (Assignment name e) ->
    Typed qt area metadata (Assignment name (reduceWithPureFns pureFns e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (reduceWithPureFns pureFns e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (reduceWithPureFns pureFns fn) (reduceWithPureFns pureFns <$> args))

  Typed qt area metadata (Definition params body) ->
    let reducedBody = reduceWithPureFns pureFns <$> body
        fusedBody = fuseMapRepeatInBody pureFns reducedBody
        cleanedBody = eliminateUnusedAssignmentsInBody pureFns fusedBody
    in Typed qt area metadata (Definition params cleanedBody)

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem (reduceWithPureFns pureFns) <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (reduceWithPureFns pureFns <$> items))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (reduceWithPureFns pureFns rec) (reduceWithPureFns pureFns field))

  Typed qt area metadata (ArrayAccess arr index) ->
    Typed qt area metadata (ArrayAccess (reduceWithPureFns pureFns arr) (reduceWithPureFns pureFns index))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField (reduceWithPureFns pureFns) <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (reduceWithPureFns pureFns cond) (reduceWithPureFns pureFns truthy) (reduceWithPureFns pureFns falsy))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (reduceWithPureFns pureFns cond) (reduceWithPureFns pureFns body))

  Typed qt area metadata (Do exps) ->
    let reducedExps = reduceWithPureFns pureFns <$> exps
        fusedExps = fuseMapRepeatInBody pureFns reducedExps
        cleanedExps = eliminateUnusedAssignmentsInBody pureFns fusedExps
    in Typed qt area metadata (Do cleanedExps)

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (reduceWithPureFns pureFns exp) (reduceIsWithPureFns pureFns <$> iss))

  _ ->
    exp


reduceAST :: AST -> AST
reduceAST ast =
  let pureFns = collectPureUnaryTopLevelFns (aexps ast)
      reducedExps = reduceWithPureFns pureFns <$> aexps ast
  in  ast { aexps = reducedExps }
