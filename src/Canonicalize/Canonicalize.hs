{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.Canonicalize where

import           Target
import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Canonicalize.JSExp
import           Canonicalize.CanonicalM
import qualified Canonicalize.Env              as E
import           Canonicalize.Typing
import qualified Data.Map                      as M



class Canonicalizable a b where
  canonicalize :: E.Env -> Target -> a -> CanonicalM b


instance Canonicalizable Src.Exp Can.Exp where
  canonicalize env target (Src.Source infos area e) = case e of
    Src.LNum  x           -> return $ Can.Canonical area (Can.LNum x)

    Src.LStr  x           -> return $ Can.Canonical area (Can.LStr x)

    Src.LBool x           -> return $ Can.Canonical area (Can.LBool x)

    Src.LUnit             -> return $ Can.Canonical area Can.LUnit

    Src.TemplateString es -> do
      es' <- mapM (canonicalize env target) es
      return $ Can.Canonical area (Can.TemplateString es')

    Src.JSExp js ->
      return $ Can.Canonical area (Can.JSExp $ filterJSExp target js)

    Src.App fn arg close -> do
      fn'  <- canonicalize env target fn
      arg' <- canonicalize env target arg
      return $ Can.Canonical area (Can.App fn' arg' close)

    Src.FieldAccess rec field -> do
      rec'   <- canonicalize env target rec
      field' <- canonicalize env target field
      return $ Can.Canonical area (Can.FieldAccess rec' field')

    Src.NamespaceAccess n ->
      return $ Can.Canonical area (Can.NamespaceAccess n)

    Src.Abs param body -> do
      body' <- mapM (canonicalize env target) body
      return $ Can.Canonical area (Can.Abs param body')

    Src.Assignment name exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Assignment name exp')

    Src.Export exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Export exp')

    Src.Var name            -> return $ Can.Canonical area (Can.Var name)

    Src.TypedExp exp typing -> do
      exp'   <- canonicalize env target exp
      scheme <- typingToScheme env typing
      return $ Can.Canonical area (Can.TypedExp exp' scheme)

    Src.ListConstructor items -> do
      items' <- mapM (canonicalize env target) items
      return $ Can.Canonical area (Can.ListConstructor items')

    Src.TupleConstructor exps -> do
      exps' <- mapM (canonicalize env target) exps
      return $ Can.Canonical area (Can.TupleConstructor exps')

    Src.Record fields -> do
      fields' <- mapM (canonicalize env target) fields
      return $ Can.Canonical area (Can.Record fields')

    Src.If cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.Where exp iss -> do
      exp' <- canonicalize env target exp
      iss' <- mapM (canonicalize env target) iss
      return $ Can.Canonical area (Can.Where exp' iss')

    Src.Pipe exps -> do
      app <- buildApplication (Can.Canonical area (Can.Var "_P_")) exps
      return $ Can.Canonical area (Can.Abs "_P_" [app])

     where
      buildApplication :: Can.Exp -> [Src.Exp] -> CanonicalM Can.Exp
      buildApplication prev es = case es of
        [e] -> do
          e' <- canonicalize env target e
          return $ Can.Canonical (Can.getArea e') (Can.App e' prev True)

        e : es -> do
          e' <- canonicalize env target e
          let app = Can.Canonical (Can.getArea e') (Can.App e' prev True)
          buildApplication app es


instance Canonicalizable Src.ListItem Can.ListItem where
  canonicalize env target item = case item of
    Src.ListItem exp -> do
      exp' <- canonicalize env target exp
      return $ Can.ListItem exp'

    Src.ListSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.ListSpread exp'


instance Canonicalizable Src.Field Can.Field where
  canonicalize env target item = case item of
    Src.Field (name, exp) -> do
      exp' <- canonicalize env target exp
      return $ Can.Field (name, exp')

    Src.FieldSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.FieldSpread exp'


instance Canonicalizable Src.Is Can.Is where
  canonicalize env target (Src.Source _ area (Src.Is pat exp)) = do
    pat' <- canonicalize env target pat
    exp' <- canonicalize env target exp
    return $ Can.Canonical area (Can.Is pat' exp')


instance Canonicalizable Src.Pattern Can.Pattern where
  canonicalize env target (Src.Source _ area pat) = case pat of
    Src.PVar name       -> return $ Can.Canonical area (Can.PVar name)

    Src.PAny            -> return $ Can.Canonical area Can.PAny

    Src.PCtor name pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PCtor name pats')

    Src.PNum    num  -> return $ Can.Canonical area (Can.PNum num)

    Src.PStr    str  -> return $ Can.Canonical area (Can.PStr str)

    Src.PBool   boo  -> return $ Can.Canonical area (Can.PBool boo)

    Src.PCon    name -> return $ Can.Canonical area (Can.PCon name)

    Src.PRecord pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PRecord pats')

    Src.PList pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PList pats')

    Src.PTuple pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PTuple pats')

    Src.PSpread pat -> do
      pat' <- canonicalize env target pat
      return $ Can.Canonical area (Can.PSpread pat')


instance Canonicalizable Src.Import Can.Import where
  canonicalize env target (Src.Source _ area imp) = case imp of
    Src.NamedImport names relPath absPath ->
      return $ Can.Canonical area (Can.NamedImport names relPath absPath)

    Src.DefaultImport namespace relPath absPath ->
      return $ Can.Canonical area (Can.DefaultImport namespace relPath absPath)
