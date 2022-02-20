module Optimize.TCE where

import           AST.Core
import qualified Data.Maybe          as Maybe

newtype Env
  = Env { envCurrentName :: Maybe String }



getAppName :: Exp -> Maybe String
getAppName exp = case exp of
  Typed _ _ _ (Var n) ->
    Just n

  Typed _ _ _ (Call fn _) ->
    getAppName fn

  Typed _ _ _ (Placeholder _ e) ->
    getAppName e

  _ ->
    Nothing



resolve :: AST -> AST
resolve ast =
  let resolved = markDefinition (Env Nothing) <$> aexps ast
  in  ast { aexps = resolved }


markDefinition :: Env -> Exp -> Exp
markDefinition env exp = case exp of
  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (markDefinition env e))

  Typed qt area metadata (Assignment fnName abs) ->
    Typed qt area metadata (Assignment fnName (markDefinition env { envCurrentName = Just fnName } abs))

  Typed qt area metadata (Definition params body) | Maybe.isJust (envCurrentName env) ->
    let Just fnName = envCurrentName env
    in  case findRecursionKind fnName params body of
          Just kind ->
            Typed qt area (RecursiveDefinition kind : metadata) (Definition params (markTRCCalls kind fnName . markDefinition env <$> body))
          Nothing ->
            Typed qt area metadata (Definition params (markDefinition env <$> body))

  Typed qt area metadata (Placeholder ref exp) ->
    Typed qt area metadata (Placeholder ref (markDefinition env exp))

  _ ->
    exp


markIs :: RecursionKind -> String -> Is -> Is
markIs recursionKind fnName is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (markTRCCalls recursionKind fnName exp))

  _ ->
    is

-- looks for tail recursive calls and marks them
markTRCCalls :: RecursionKind -> String -> Exp -> Exp
markTRCCalls recursionKind fnName exp = case exp of
  Typed qt area metadata (Call fn args) ->
    if Just fnName == getAppName exp then
      Typed qt area (RecursiveCall PlainRecursion : metadata) (Call fn args)
    else
      Typed qt area (RecursionEnd recursionKind : metadata) (Call fn args)

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If cond (markTRCCalls recursionKind fnName truthy) (markTRCCalls recursionKind fnName falsy))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where exp (markIs recursionKind fnName <$> iss))

  -- TODO: Probably we need to either mark the whole thing, or just the last expression of it
  -- Typed qt area metadata (Do exps) ->
  --   Typed qt area metadata (Do (markTRCCalls markWith fnName <$> exps))

  Typed qt area metadata (Placeholder info exp) ->
    Typed qt area metadata (Placeholder info (markTRCCalls recursionKind fnName exp))

  Typed qt area metadata (ListConstructor [Typed qtLi areaLi metadataLi (ListItem li), Typed qtSpread areaSpread metadataSpread (ListSpread spread)]) ->
    -- we probably need to mark this node as being ListRecursion
    if containsRecursion True fnName spread then
      Typed qt area (RecursiveCall (ListRecursion RightRecursion) : metadata) (ListConstructor [
        Typed qtLi areaLi metadataLi (ListItem li),
        Typed qtSpread areaSpread metadataSpread (ListSpread (markTRCCalls recursionKind fnName spread))
      ])
    else
      Typed qt area metadata (ListConstructor [
        Typed qtLi areaLi metadataLi (ListItem li),
        Typed qtSpread areaSpread metadataSpread (ListSpread spread)
      ])

  Typed qt area metadata e ->
    Typed qt area (RecursionEnd recursionKind : metadata) e
    -- if recursionKind /= PlainRecursion && recursionKind /= NotOptimizable then
      -- Typed qt area (RecursionEnd recursionKind : metadata) e
    -- else
      -- Typed qt area metadata e

  _ ->
    exp


combineRecursionKinds :: [Maybe RecursionKind] -> Maybe RecursionKind
combineRecursionKinds kinds = case kinds of
  (Just (ListRecursion side) : more) ->
    Just (ListRecursion side)

  (Just PlainRecursion : more) ->
    case combineRecursionKinds more of
      Nothing ->
        Just PlainRecursion

      or ->
        or

  (Nothing : more) ->
    combineRecursionKinds more

  _ ->
    Nothing




findRecursionKindInIss :: String -> [String] -> [Is] -> Maybe RecursionKind
findRecursionKindInIss fnName params iss = case iss of
  [] ->
    Just PlainRecursion

  [Typed _ _ _ (Is _ isExp)] ->
    if containsRecursion True fnName isExp then
      Just PlainRecursion
    else
      findRecursionKind fnName params [isExp]

  (Typed _ _ _ (Is _ isExp) : next) ->
    let current =
          if containsRecursion True fnName isExp then
            Just PlainRecursion
          else
            findRecursionKind fnName params [isExp]
        next' = findRecursionKindInIss fnName params next
    in  combineRecursionKinds [current, next']

  _ ->
    Nothing


findRecursionKind :: String -> [String] -> [Exp] -> Maybe RecursionKind
findRecursionKind fnName params exps = case exps of
  [] ->
    Nothing

  [lastExp] -> case lastExp of
    Typed _ _ _ Call {} | containsRecursion True fnName lastExp ->
      Just PlainRecursion

    Typed _ _ _ (If cond truthy falsy) ->
       combineRecursionKinds
          [ findRecursionKind fnName params [truthy]
          , findRecursionKind fnName params [falsy]
          ]

    Typed _ _ _ (Where exp iss) ->
      findRecursionKindInIss fnName params iss

    Typed _ _ _ (ListConstructor [Typed _ _ _ (ListItem li), Typed _ _ _ (ListSpread spread)]) ->
      if not (containsRecursion False fnName li) && containsRecursion True fnName spread then
        Just (ListRecursion RightRecursion)
      else
        Nothing

    _ ->
      Nothing

  (exp : next) ->
    findRecursionKind fnName params next


containsRecursion :: Bool -> String  -> Exp -> Bool
containsRecursion direct fnName exp = case exp of
  Typed _ _ _ Call {} ->
    Just fnName == getAppName exp

  Typed _ _ _ (Access rec accessor) ->
    not direct && (containsRecursion direct fnName rec || containsRecursion direct fnName accessor)

  Typed _ _ _ (Definition _ exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ _ (Assignment _ exp) ->
    not direct && containsRecursion direct fnName exp

  Typed _ _ _ (ListConstructor exps) ->
    not direct && any (containsRecursion direct fnName) (getListItemExp <$> exps)

  Typed _ _ _ (TupleConstructor exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ _ (Record fields) ->
    not direct && any (containsRecursion direct fnName) (getFieldExp <$> fields)

  Typed _ _ _ (If cond truthy falsy) ->
    not direct &&
      (
        containsRecursion direct fnName cond
        || containsRecursion direct fnName truthy
        || containsRecursion direct fnName falsy
      )

  Typed _ _ _ (Do exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ _ (Where exp iss) ->
    not direct &&
      (
        containsRecursion direct fnName exp
        || any (containsRecursion direct fnName) (getIsExpression <$> iss)
      )

  Typed _ _ _ (Placeholder _ exp) ->
    not direct && containsRecursion direct fnName exp

  _ ->
    False
