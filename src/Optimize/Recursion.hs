module Optimize.Recursion where

import AST.Core
import Infer.Type
import Explain.Location
import Text.Show.Pretty
import Debug.Trace


convertTable :: Table -> Table
convertTable table = convertAST <$> table


convertAST :: AST -> AST
convertAST ast =
  ast { aexps = concat $ convertIfNeeded <$> aexps ast }


arityToParamNames :: Int -> [String]
arityToParamNames arity =
  take arity ((: "_0") <$> ['a'..])


arityToFnType :: Int -> Type
arityToFnType arity =
  foldr1 fn (tVar <$> arityToParamNames (arity + 1))




generateTrampolinedWrapper :: Int -> String -> Qual Type -> Exp
generateTrampolinedWrapper arity fnName qt@(_ :=> t) =
  let params         = arityToParamNames arity
      trampolineArgs = Typed ([] :=> arityToFnType arity) emptyArea (Var $ cpsName fnName) : (Typed ([] :=> tVar "a") emptyArea . Var <$> params)
  in  Typed qt emptyArea (Assignment fnName (
        Typed qt emptyArea (Definition BasicDefinition params [
          Typed ([] :=> getReturnType t) emptyArea (
            Call
              SimpleCall
              (Typed ([] :=> arityToFnType (arity + 1)) emptyArea (Var "madlib__recursion__internal__trampoline__1"))
              trampolineArgs
          )
        ])
      ))


-- Converts a recursive function to the following.
--
-- from:
-- factorial = (n) =>
--   n == 0
--     ? 1
--     : fact(n - 1) * n
--
-- to:
-- const Done = (x) => ({ __constructor: "Done", __args: [x] })
-- const Next = (f) => ({ __constructor: "Next", __args: [f] })
-- const Thunk = (f) => ({ __constructor: "Thunk", __args: [f] })


-- const __factorial__ = (n, k) =>
--   n == 0
--     ? Done(k(1))
--     : Next(() => __factorial__(n - 1, (value) => Thunk(() => k(n * value))))


-- const factorial = (n) => trampoline1(__factorial__, n)


-- const trampoline1 = (f, arg) => {
--   v = f(arg, (x) => x)

--   while (v.__constructor !== "Done") {
--     v = v.__args[0]()
--   }

--   v = v.__args[0]

--   while (v.__constructor === "Thunk") {
--     v = v.__args[0]()
--   }

--   return v
-- }
convertIfNeeded :: Exp -> [Exp]
convertIfNeeded exp =
  let fnData =
        case exp of
          Typed _ _ (Assignment fnName (Typed _ _ (Definition _ params exps))) ->
            if any (isRecursive (length params) fnName) exps then
              Just (length params, fnName, exp)
            else
              Nothing

          _ ->
            Nothing

  in  case fnData of
        Just (arity, fnName, definitionExp) ->
          [convert fnName arity definitionExp, generateTrampolinedWrapper arity fnName (getQualType definitionExp)]

        Nothing ->
          [exp]


callbackQualType :: Qual Type
callbackQualType =
  [] :=> (tUnit `fn` tVar "b")

nextQualType :: Qual Type
nextQualType =
  [] :=> ((tVar "a" `fn` tVar "b") `fn` tVar "c")

continuationType :: Type
continuationType =
  tVar "a" `fn` tVar "b"

continuationQualType :: Qual Type
continuationQualType =
  [] :=> continuationType

trampolineQualType :: Qual Type
trampolineQualType =
  [] :=> TCon (TC "Trampoline" Star) "internal"


data WrapperType
  = Thunk
  | Done
  | Next

wrap :: WrapperType -> Exp -> Exp
wrap wrapperType exp = case wrapperType of
  Next ->
    Typed trampolineQualType emptyArea (Call SimpleCall (Typed nextQualType emptyArea (Var "madlib__recursion__internal__Next")) [
      Typed callbackQualType emptyArea (Definition BasicDefinition ["__skip__"] [exp])
    ])

  Done ->
    Typed ([] :=> tVar "a") emptyArea (Call SimpleCall (Typed nextQualType emptyArea (Var "madlib__recursion__internal__Done")) [Typed ([] :=> tVar "a") emptyArea (Call SimpleCall kExp [exp])])

  _ ->
    undefined

kParamName :: String
kParamName =
  "__$kParam__"

kName :: String
kName =
  "__$k__"

kExp :: Exp
kExp =
  Typed continuationQualType emptyArea (Var kName)

varExp :: String -> Exp
varExp name =
  Typed ([] :=> tVar "a") emptyArea (Var name)

thunkifiedKExp :: Exp
thunkifiedKExp =
  Typed callbackQualType emptyArea (Definition BasicDefinition [kParamName] [
    Typed trampolineQualType emptyArea (Call SimpleCall (Typed nextQualType emptyArea (Var "madlib__recursion__internal__Thunk")) [
      Typed callbackQualType emptyArea (Definition BasicDefinition ["__skip__"] [varExp kParamName])
    ])
  ])

thunkifyKExp :: Exp -> Exp
thunkifyKExp exp =
  Typed ([] :=> (getType exp `fn` tVar "b")) emptyArea (Definition BasicDefinition [kParamName] [
    Typed trampolineQualType emptyArea (Call SimpleCall (Typed nextQualType emptyArea (Var "madlib__recursion__internal__Thunk")) [
      Typed callbackQualType emptyArea (Definition BasicDefinition ["__skip__"] [Typed ([] :=> tVar "a") emptyArea (Call SimpleCall kExp [exp])])
    ])
  ])


cpsName :: String -> String
cpsName name =
  "__" <> name <> "_CPS__"

buildFnTypeWithContinuation :: Type -> Type
buildFnTypeWithContinuation t =
  let returnType = getReturnType t
      paramTypes = getParamTypes t
  in  foldr1 fn paramTypes `fn` (tVar "a" `fn` tVar "b") `fn` returnType

convert :: String -> Int -> Exp -> Exp
convert fnName arity exp = case exp of
  Typed qt area (Call callType (Typed qt' area' (Var fnName')) args) ->
    if fnName /= fnName' then
      Typed qt area (Call callType (Typed qt' area' (Var fnName')) (convert fnName arity <$> args))
    else
      wrap
        Next
        (Typed qt area (Call callType (Typed qt' area' (Var $ cpsName fnName')) ((convert fnName arity <$> args) ++ [thunkifiedKExp])))

  -- TODO: temporary, we need to properly look for leafs in if and where exps
  Typed qt area (Call _ _ _) ->
    exp

  Typed qt area (Assignment name (Typed qt' area' (Definition defType params args))) ->
    if name == fnName then
      let returnType = getReturnType (getQualified qt')
          newTypes   = getParamTypes (getQualified qt') ++ [continuationType, returnType]
          newType    = preds qt' :=> foldr1 fn newTypes
      in  Typed newType area (Assignment (cpsName name) (Typed newType area' (Definition defType (params ++ [kName]) (convert fnName arity <$> args))))
    else
      Typed qt area (Assignment name (Typed qt' area' (Definition defType params (convert fnName arity <$> args))))

  Typed qt area (Assignment name e) ->
    Typed qt area (Assignment name (convert fnName arity e))

  Typed qt area (Definition defType params args) ->
    Typed qt area (Definition defType params (convert fnName arity <$> args))

  Typed qt area (If cond truthy falsy) ->
    -- let ()
    -- in  undefined
    Typed qt area (If
      cond
      (convert fnName arity truthy)
      (convertLeaf fnName arity falsy)
      -- (convert fnName arity falsy)
    )

  _ ->
    wrap Done exp


convertLeaf :: String -> Int -> Exp -> Exp
convertLeaf fnName arity exp = case searchForRecursion fnName arity exp of
  (converted, Recursive args qt) ->
    wrap
      Next
      (Typed (preds qt :=> getReturnType (getQualified qt)) emptyArea (Call SimpleCall (Typed qt emptyArea (Var $ cpsName fnName)) (args ++ [thunkifyKExp converted])))

  (_, Final) ->
    wrap Done exp

  (_, NotFinal) ->
    exp


data RecursionSearchResult
  = Recursive [Exp] (Qual Type)
  | Final
  | NotFinal


searchArgs :: String -> Int -> [Exp] -> ([Exp], RecursionSearchResult)
searchArgs fnName arity exps = case exps of
  (exp : more) ->
    case searchForRecursion fnName arity exp of
      (exp', Recursive args qt) ->
        (exp' : more, Recursive args qt)

      (exp', _) ->
        let (more', result) = searchArgs fnName arity more
        in  (exp : more', result)

  [] ->
    ([], NotFinal)

searchForRecursion :: String -> Int -> Exp -> (Exp, RecursionSearchResult)
searchForRecursion fnName arity exp = case exp of
  Typed qt area (Call callType fn args) ->
    case fn of
      Typed qt' area' (Var name) | name == fnName ->
        (Typed qt area' (Var kParamName), Recursive args qt')

      _ ->
        let nextFn             = fst $ searchForRecursion fnName arity fn
            (nextArgs, result) = searchArgs fnName arity args
        in  (Typed qt area (Call callType nextFn nextArgs), result)

  _ ->
    (exp, NotFinal)


isRecursive :: Int -> String -> Exp -> Bool
isRecursive arity functionName exp = case exp of
  Typed _ _ (Call _ (Typed _ _ (Var name)) args) | name == functionName && arity == length args ->
    True

  Typed _ _ (Call _ fn args) ->
    isRecursive arity functionName fn || any (isRecursive arity functionName) args

  Typed _ _ (Access rec _) ->
    isRecursive arity functionName rec

  Typed _ _ (Assignment _ e) ->
    isRecursive arity functionName e

  Typed _ _ (ListConstructor items) ->
    any (isRecursive arity functionName) (getListItemExp <$> items)

  Typed _ _ (Record fields) ->
    any (isRecursive arity functionName) (getFieldExp <$> fields)

  Typed _ _ (TupleConstructor items) ->
    any (isRecursive arity functionName) items

  Typed _ _ (If cond truthy falsy) ->
    isRecursive arity functionName cond
    || isRecursive arity functionName truthy
    || isRecursive arity functionName falsy

  Typed _ _ (Do exps) ->
    any (isRecursive arity functionName) exps

  Typed _ _ (Where e iss) ->
    isRecursive arity functionName e
    || any (isRecursive arity functionName) (getIsExpression <$> iss)

  Typed _ _ (Placeholder _ e) ->
    isRecursive arity functionName e

  _ ->
    False

