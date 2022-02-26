module Optimize.EtaExpansion where

import AST.Core
import Infer.Type
import Explain.Location
import qualified Data.Bifunctor as Bifunctor


expandTable :: Table -> Table
expandTable = (expandAST <$>)


expandAST :: AST -> AST
expandAST ast =
  let expandedExps = expand <$> aexps ast
      expandedInstances =
        (
          \(Untyped area metadata (Instance name ps n methods)) ->
             let expandedMethods = Bifunctor.first expand <$> methods
             in  Untyped area metadata (Instance name ps n expandedMethods)
        ) <$> ainstances ast
  in  ast { aexps = expandedExps, ainstances = expandedInstances }


isDefinition :: Exp -> Bool
isDefinition exp = case exp of
  Typed _ _ _ (Placeholder (ClassRef _ _ False True, _) wrapped) ->
    isDefinition wrapped

  Typed _ _ _ (Definition _ _) ->
    True

  _ ->
    False


makeParamNames :: Int -> [String]
makeParamNames count = ("$param_" <>) . (: "") <$> take count ['a'..]


buildExpandedBody :: [(String, Qual Type)] -> Exp -> Exp
buildExpandedBody argInfo exp =
  let expQualType = getQualType exp
      returnType  = getReturnType . getQualified $ expQualType
      ps          = selectPredsForType (preds expQualType) returnType
      args        = (\(argName, argQt) -> Typed argQt emptyArea [] (Var argName False)) <$> argInfo
  in  case exp of
        Typed qt area metadata (Var name isConstructor) ->
          Typed (ps :=> returnType) area [] (Call (Typed qt area metadata (Var name isConstructor)) args)

        Typed qt area metadata (Placeholder ref@(MethodRef{}, _) _) ->
          Typed (ps :=> returnType) area [] (Call exp args)

        Typed qt area metadata (Call fn args') ->
          Typed (ps :=> returnType) area metadata (Call fn (args' ++ args))

        Typed qt area metadata (Placeholder ref@(ClassRef{}, _) wrapped) ->
          Typed qt area metadata (Placeholder ref (buildExpandedBody argInfo wrapped))

        _ ->
          exp


updatePlaceholderWrappedExp :: (Exp -> Exp) -> Exp -> Exp
updatePlaceholderWrappedExp update ph = case ph of
  Typed qt area metadata (Placeholder ref@(ClassRef{}, _) next) ->
    Typed qt area metadata (Placeholder ref (updatePlaceholderWrappedExp update next))

  _ ->
    update ph


expandIs :: Is -> Is
expandIs is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (expand exp))

  _ ->
    is


expand :: Exp -> Exp
expand exp = case exp of
  Typed qt area metadata (Assignment name e) ->
    if isDefinition e || not (isFunctionType (getQualified qt)) then
      Typed qt area metadata (Assignment name (expand e))
    else
      let paramTypes     = getParamTypes . getQualified $ qt
          paramQualTypes = (\t -> selectPredsForType (preds qt) t :=> t) <$> paramTypes
          paramNames     = makeParamNames (length paramTypes)
          definition     = updatePlaceholderWrappedExp (\e' -> Typed qt area [] (Definition paramNames [buildExpandedBody (zip paramNames paramQualTypes) e'])) e
      in  Typed qt area metadata (Assignment name definition)

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (expand e))

  Typed qt area metadata (Var name isConstructor) ->
    if not (isFunctionType (getQualified qt)) then
      exp
    else
      let paramTypes     = getParamTypes . getQualified $ qt
          paramQualTypes = (\t -> selectPredsForType (preds qt) t :=> t) <$> paramTypes
          paramNames     = makeParamNames (length paramTypes)
          definition     = updatePlaceholderWrappedExp (\e' -> Typed qt area [] (Definition paramNames [buildExpandedBody (zip paramNames paramQualTypes) e'])) exp
      in  definition

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call fn (expand <$> args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (expand <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem expand <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (expand <$> items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField expand <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (expand cond) (expand truthy) (expand falsy))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (expand <$> exps))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (expand exp) (expandIs <$> iss))

  Typed qt area metadata (Placeholder ref exp) ->
    Typed qt area metadata (Placeholder ref (expand exp))

  _ ->
    exp
