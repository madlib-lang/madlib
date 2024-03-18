{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Infer.Placeholder where

import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Infer.Pred
import           Infer.Unify
import           Infer.Substitute
import qualified AST.Solved                    as Slv
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.Except
import           Error.Error
import           Error.Context
import           Infer.Instantiate
import           Run.Options
import           Error.Warning
import Text.Show.Pretty (ppShow)


{-
Post type inference traversing of a top level expression or method. It mainly does one thing,
which is assuring consistency of generated placeholders and update. Because only after
generalization and instance resolution we have complete knowledge of the types for the placeholder
types, we need to go through the tree again to update these and generate appropriate types.

For example we may have a parameter placeholder being fully resolved like:
(Show_a) => ... becomes (Show_Integer) => ...
In that case we can completely strip it.

Or we may have multiple levels of a dictionary, in which case we simply strip the inner one as
it's already being captured in the current scope, example:
(Show_a, a) => {
  return (Show_a, a) => ...
}
In that case we can simply remove the inner one:
(Show_a) => {
  return (a) => ...
}
The codegen would then capture the usage of it via findFreeVars during closure conversion.


The main entrypoint of the module is updateExpTypes, which mainly traverses the AST and calls
the updateMethod/ClassPlaceholder whenever a placeholder is encountered.
-}


verifyMethodAccess :: Env -> Substitution -> Slv.Exp -> Infer Slv.Exp
verifyMethodAccess env s ph@(Slv.Typed (ps :=> t) a (Slv.Var methodName _)) = do
  forM_ ps $ \(IsIn cls instanceTypes _) -> do
    let instanceTypes' = apply s instanceTypes
    -- The following block serves to check that the inferred type for a method
    -- which was unified in type check with the class' scheme, is actually a
    -- type that is correct, given the actual instance's specific type.
    maybeInst <- findInst env (IsIn cls instanceTypes' Nothing)
    case maybeInst of
      Just (Instance _ methods) -> case M.lookup methodName methods of
        Just methodScheme -> do
          (_ :=> mtdT) <- instantiate methodScheme
          catchError
            (match mtdT (apply s t))
            (\(CompilationError e _) ->
              throwError $ CompilationError e (Context (envCurrentPath env) a)
            )
        Nothing -> return mempty
      Nothing -> return mempty

  return ph


updateExpTypesForExpList :: Options -> Env -> Bool -> Substitution -> [Slv.Exp] -> Infer [Slv.Exp]
updateExpTypesForExpList options env push s exps = case exps of
  (e : es) -> do
    es' <- updateExpTypesForExpList options env push s es
    e' <- updateExpTypes options env push s e
    return (e' : es')

  [] ->
    return []


updateExpTypes :: Options -> Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateExpTypes _ _ _ _ fullExp@(Slv.Untyped _ _)   = return fullExp
updateExpTypes options env push s fullExp@(Slv.Typed qt a e) = case e of
  Slv.Var name isCtor -> do
    let isMethod = M.member name (envMethods env)
    when isMethod $ do
      verifyMethodAccess env s fullExp
      return ()

    return $ Slv.Typed (apply s qt) a (Slv.Var name isCtor)

  Slv.App abs arg final -> do
    abs' <- updateExpTypes options env push s abs
    arg' <- updateExpTypes options env push s arg
    return $ Slv.Typed (apply s qt) a $ Slv.App abs' arg' final

  Slv.Abs (Slv.Typed paramType paramArea param) es -> do
    es' <- updateExpTypesForExpList options env push s es
    let param' = Slv.Typed (apply s paramType) paramArea param
    return $ Slv.Typed (apply s qt) a $ Slv.Abs param' es'

  Slv.Do exps -> do
    exps' <- updateExpTypesForExpList options env push s exps
    return $ Slv.Typed (apply s qt) a $ Slv.Do exps'

  Slv.Where exp iss -> do
    exp' <- updateExpTypes options env push s exp
    iss' <- mapM (updateIs s) iss
    return $ Slv.Typed (apply s qt) a $ Slv.Where exp' iss'

  Slv.Assignment n exp -> do
    exp' <- updateExpTypes options env push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.Assignment n exp'

  Slv.Mutate lhs exp -> do
    lhs' <- updateExpTypes options env push s lhs
    exp' <- updateExpTypes options env push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.Mutate lhs' exp'

  Slv.ListConstructor li -> do
    li' <- mapM (updateListItem s) li
    return $ Slv.Typed (apply s qt) a $ Slv.ListConstructor li'

  Slv.TypedExp exp typing sc -> do
    exp' <- updateExpTypes options env push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.TypedExp exp' typing sc

  Slv.Export exp -> do
    exp' <- updateExpTypes options env push s exp
    return $ Slv.Typed (apply s qt) a $ Slv.Export exp'

  Slv.If econd eif eelse -> do
    econd' <- updateExpTypes options env push s econd
    eif'   <- updateExpTypes options env push s eif
    eelse' <- updateExpTypes options env push s eelse
    return $ Slv.Typed (apply s qt) a $ Slv.If econd' eif' eelse'

  Slv.While econd ebody -> do
    econd' <- updateExpTypes options env push s econd
    ebody'   <- updateExpTypes options env push s ebody
    return $ Slv.Typed (apply s qt) a $ Slv.While econd' ebody'

  Slv.TupleConstructor es -> do
    es' <- mapM (updateExpTypes options env push s) es
    return $ Slv.Typed (apply s qt) a $ Slv.TupleConstructor es'

  Slv.TemplateString es -> do
    es' <- mapM (updateExpTypes options env push s) es
    return $ Slv.Typed (apply s qt) a $ Slv.TemplateString es'

  Slv.Access rec field -> do
    rec'   <- updateExpTypes options env push s rec
    field' <- updateExpTypes options env push s field
    return $ Slv.Typed (apply s qt) a $ Slv.Access rec' field'

  Slv.ArrayAccess arr index -> do
    arr'   <- updateExpTypes options env push s arr
    index' <- updateExpTypes options env push s index
    return $ Slv.Typed (apply s qt) a $ Slv.ArrayAccess arr' index'

  Slv.Record fields -> do
    fields' <- mapM (updateField s) fields
    let appliedQt = apply s qt
    case appliedQt of
      _ :=> TRecord fieldTypes _ _ ->
        pushExtensibleRecordToDerive $ M.keys fieldTypes

      _ ->
        return ()
    return $ Slv.Typed appliedQt a $ Slv.Record fields'

  Slv.TypedHole -> do
    let qt'@(_ :=> t) = apply s qt
    pushWarning $ CompilationWarning (TypedHoleFound t) (Context (envCurrentPath env) a)
    return $ Slv.Typed qt' a Slv.TypedHole

  _ ->
    return $ Slv.Typed (apply s qt) a e

  where
    updateIs :: Substitution -> Slv.Is -> Infer Slv.Is
    updateIs s (Slv.Typed qt@(ps :=> _) a is) = case is of
      Slv.Is pat exp -> do
        exp' <- updateExpTypes options env push s exp
        return $ Slv.Typed (apply s qt) a $ Slv.Is (updatePattern s ps pat) exp'

    updatePattern :: Substitution -> [Pred] -> Slv.Pattern -> Slv.Pattern
    updatePattern s preds pat@(Slv.Typed (_ :=> t) _ _) =
      let ps = selectPredsForType preds t
      in  case pat of
        Slv.Typed (ps' :=> t') area (Slv.PCon n pats) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PCon n (updatePattern s preds <$> pats))

        Slv.Typed (ps' :=> t') area (Slv.PRecord fields) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PRecord (updatePattern s preds <$> fields))

        Slv.Typed (ps' :=> t') area (Slv.PList items) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PList (updatePattern s preds <$> items))

        Slv.Typed (ps' :=> t') area (Slv.PTuple items) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PTuple (updatePattern s preds <$> items))

        Slv.Typed (ps' :=> t') area (Slv.PSpread pat) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PSpread (updatePattern s preds pat))

        Slv.Typed (ps' :=> t') area (Slv.PVar n) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PVar n)

        Slv.Typed (ps' :=> t') area (Slv.PNum n) ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area (Slv.PNum n)

        Slv.Typed (ps' :=> t') area p ->
          Slv.Typed (apply s ((ps ++ ps') :=> t')) area p


    updateListItem :: Substitution -> Slv.ListItem -> Infer Slv.ListItem
    updateListItem s (Slv.Typed t area li) = case li of
      Slv.ListItem e ->
        Slv.Typed (apply s t) area . Slv.ListItem <$> updateExpTypes options env push s e

      Slv.ListSpread e ->
        Slv.Typed (apply s t) area . Slv.ListSpread <$> updateExpTypes options env push s e

    updateField :: Substitution -> Slv.Field -> Infer Slv.Field
    updateField s (Slv.Typed t area field) = case field of
      Slv.Field       (n, e) -> Slv.Typed (apply s t) area . Slv.Field . (n, ) <$> updateExpTypes options env push s e
      Slv.FieldSpread e      -> Slv.Typed (apply s t) area . Slv.FieldSpread <$> updateExpTypes options env push s e
