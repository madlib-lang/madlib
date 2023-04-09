{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Optimize.ToCore where

import qualified Control.Monad.State           as MonadState
import qualified Data.Map                      as M
import           Data.List
import qualified AST.Solved                    as Slv
import qualified AST.Core                      as Core
import           Infer.Type
import           Explain.Location
import qualified Utils.Types                   as Types
import           Text.Show.Pretty


data State
  = State { typeCount  :: Int
          , classCount :: Int
          , typeMap    :: M.Map String String
          , classMap   :: M.Map String String
          }

initialOptimizationState :: State
initialOptimizationState = State { typeCount = 0, classCount = 0, typeMap = mempty, classMap = mempty }

type PostProcess a = forall m . MonadState.MonadState State m => m a

numbers :: [String]
numbers = show <$> [0 ..]

generateClassShortname :: String -> PostProcess String
generateClassShortname "Eq" = return "Eq"
generateClassShortname n = do
  s <- MonadState.get
  let shortName = 'Ι' : numbers !! (1 + classCount s)
  MonadState.put s { classCount = classCount s + 1, classMap = M.insert n shortName (classMap s) }
  return shortName

getClassShortname :: Bool -> String -> PostProcess String
getClassShortname enabled n
  | not enabled = return n
  | otherwise = do
    s <- MonadState.get
    case M.lookup n (classMap s) of
      Just x  -> return x
      Nothing -> generateClassShortname n

generateTypeShortname :: String -> PostProcess String
generateTypeShortname n = do
  s <- MonadState.get
  let shortName = 'τ' : numbers !! (1 + typeCount s)
  MonadState.put s { typeCount = typeCount s + 1, typeMap = M.insert n shortName (typeMap s) }
  return shortName

getTypeShortname :: Bool -> String -> PostProcess String
getTypeShortname enabled n
  | not enabled = return n
  | otherwise = do
    s <- MonadState.get
    case M.lookup n (typeMap s) of
      Just x  -> return x
      Nothing -> generateTypeShortname n


collectAbsParams :: Slv.Exp -> ([String], [Slv.Exp])
collectAbsParams abs = case abs of
  Slv.Typed _ _ (Slv.Abs (Slv.Typed _ _ param) [body]) ->
    let (nextParams, nextBody) = collectAbsParams body
    in  (param : nextParams, nextBody)

  Slv.Typed _ _ (Slv.Abs (Slv.Typed _ _ param) body) ->
    ([param], body)

  b ->
    ([], [b])


buildAbs :: [(String, Qual Type)] -> [Slv.Exp] -> Slv.Exp
buildAbs [(param, ps :=> t)] body =
  let bodyType = Slv.getType (last body)
  in  Slv.Typed (ps :=> (t `fn` bodyType)) emptyArea (Slv.Abs (Slv.Typed (ps :=> t) emptyArea param) body)
buildAbs ((param, ps :=> t) : xs) body =
  let next     = buildAbs xs body
      nextType = Slv.getType next
  in  Slv.Typed ([] :=> (t `fn` nextType)) emptyArea (Slv.Abs (Slv.Typed (ps :=> t) emptyArea param) [next])


buildApp :: Slv.Exp -> [Slv.Exp] -> Slv.Exp
buildApp f args =
  buildApp' (length args) (length args) f args

buildApp' :: Int -> Int -> Slv.Exp -> [Slv.Exp] -> Slv.Exp
buildApp' total nth f@(Slv.Typed (ps :=> t) area _) [arg] =
  Slv.Typed (ps :=> dropFirstParamType t) area (Slv.App f arg (total == nth))
buildApp' total nth f@(Slv.Typed (ps :=> t) _ _) xs =
  let arg@(Slv.Typed _ area _) = last xs
      subApp                    = buildApp' total (nth - 1) f (init xs)
  in  Slv.Typed (ps :=> dropNFirstParamTypes nth t) area (Slv.App subApp arg (total == nth))



-- TemplateString -------------------------------------------------------------
stringConcat :: Core.Exp
stringConcat =
  Core.Typed ([] :=> (tStr `fn` tStr `fn` tStr)) emptyArea [] (Core.Var "++" False)


templateStringToCalls :: [Core.Exp] -> Core.Exp
templateStringToCalls exps = case exps of
  [e@(Core.Typed qt area _ _), e'@(Core.Typed qt' area' _ _)] ->
    Core.Typed
      ((preds qt ++ preds qt') :=> tStr)
      (mergeAreas area area')
      []
      (Core.Call stringConcat [e, e'])

  (e@(Core.Typed qt area _ _) : e'@(Core.Typed qt' area' _ _) : next) ->
    let concatenated =
          Core.Typed
            ((preds qt ++ preds qt') :=> tStr)
            (mergeAreas area area')
            []
            (Core.Call stringConcat [e, e'])
        nextStr@(Core.Typed qt'' area'' _ _) = templateStringToCalls next
    in  Core.Typed
          ((preds qt ++ preds qt' ++ preds qt'') :=> tStr)
          (mergeAreas area area'')
          []
          (Core.Call stringConcat [concatenated, nextStr])

  [last] ->
    last

  [] ->
    Core.Typed ([] :=> tStr) emptyArea [] (Core.Literal $ Core.LStr "")



class Processable a b where
  -- Bool is True when we need to contract names for JS output
  toCore :: Bool -> a -> PostProcess b


instance Processable Slv.Exp Core.Exp where
  toCore _ (Slv.Untyped _ e)  = error $ "not implemented: " <> ppShow e
  toCore enabled fullExp@(Slv.Typed qt area e) = case e of
    Slv.LNum  x           -> return $ Core.Typed qt area [] (Core.Literal $ Core.LNum x)

    Slv.LFloat x          -> return $ Core.Typed qt area [] (Core.Literal $ Core.LFloat x)

    Slv.LStr  x           -> return $ Core.Typed qt area [] (Core.Literal $ Core.LStr x)

    Slv.LChar x           -> return $ Core.Typed qt area [] (Core.Literal $ Core.LChar x)

    Slv.LBool x           -> return $ Core.Typed qt area [] (Core.Literal $ Core.LBool x)

    Slv.LUnit             -> return $ Core.Typed qt area [] (Core.Literal Core.LUnit)

    Slv.TypedHole         -> return $ Core.Typed qt area [] Core.TypedHole

    Slv.TemplateString es -> do
      es' <- mapM (toCore enabled) es
      return $ templateStringToCalls es'

    Slv.JSExp js         -> return $ Core.Typed qt area [] (Core.JSExp js)

    Slv.App{} -> do
      let (fn', args) = Slv.collectAppArgs True fullExp
      fn''  <- toCore enabled fn'
      args' <- mapM (toCore enabled) args
      return $ Core.Typed qt area [] (Core.Call fn'' args')

    Slv.Access rec (Slv.Typed fieldQt fieldArea (Slv.Var ('.':fieldName) isConstructor)) -> do
      rec' <- toCore enabled rec
      let field' = Core.Typed fieldQt fieldArea [] (Core.Var ('.':fieldName) isConstructor)
      return $ Core.Typed qt area [] (Core.Access rec' field')

    Slv.Abs Slv.Typed{} _ -> do
      let (params, body') = collectAbsParams fullExp
          ps = preds qt
          paramTypes = getParamTypes (getQualified qt)
          paramQts = (\t -> selectPredsForType ps t :=> t) <$> paramTypes
          params' = (\(paramName, paramQt) -> Core.Typed paramQt emptyArea [] paramName) <$> zip params paramQts
      body'' <- mapM (toCore enabled) body'
      return $ Core.Typed qt area [] (Core.Definition params' body'')

    Slv.Assignment name exp -> do
      exp' <- toCore enabled exp
      return $ Core.Typed qt area [] (Core.Assignment name exp')

    Slv.Export exp -> do
      exp' <- toCore enabled exp
      return $ Core.Typed qt area [] (Core.Export exp')

    Slv.NameExport name ->
      return $ Core.Typed qt area [] (Core.NameExport name)

    Slv.Var name isConstructor ->
      case name of
        '.':fieldName -> do
          let expPreds    = preds qt
              recordType  = head (getParamTypes (getQualified qt))
              recordPreds = selectPredsForType expPreds recordType
              returnType  = getReturnType (getQualified qt)
              returnPreds = selectPredsForType expPreds returnType
          return $ Core.Typed qt area [] (Core.Definition [Core.Typed (recordPreds :=> recordType) emptyArea [] "__R__"] [
              Core.Typed (returnPreds :=> returnType) area [] (
                Core.Access
                  (Core.Typed (recordPreds :=> recordType) area [] (Core.Var "__R__" False))
                  (Core.Typed (returnPreds :=> returnType) area [] (Core.Var ('.':fieldName) False))
              )
            ])

        _ ->
          return $ Core.Typed qt area [] (Core.Var name isConstructor)

    Slv.TypedExp exp _ _ -> do
      toCore enabled exp

    Slv.ListConstructor items -> do
      items' <- mapM (toCore enabled) items
      return $ Core.Typed qt area [] (Core.ListConstructor items')

    Slv.TupleConstructor exps -> do
      exps' <- mapM (toCore enabled) exps
      return $ Core.Typed qt area [] (Core.TupleConstructor exps')

    Slv.Record fields -> do
      fields' <- mapM (toCore enabled) fields
      return $ Core.Typed qt area [] (Core.Record fields')

    Slv.If cond truthy falsy -> do
      cond'   <- toCore enabled cond
      truthy' <- toCore enabled truthy
      falsy'  <- toCore enabled falsy
      return $ Core.Typed qt area [] (Core.If cond' truthy' falsy')

    Slv.Do exps -> do
      exps' <- mapM (toCore enabled) exps
      return $ Core.Typed qt area [] (Core.Do exps')

    Slv.Where exp iss -> do
      exp' <- toCore enabled exp
      iss' <- mapM (toCore enabled) iss
      return $ Core.Typed qt area [] (Core.Where exp' iss')

    Slv.Extern qt name foreignName ->
      return $ Core.Typed qt area [] (Core.Extern qt name foreignName)

    Slv.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- toCore enabled exp
      placeholderRef' <- optimizePlaceholderRef placeholderRef
      let tsStr = Types.buildTypeStrForPlaceholder ts
      ts' <- getTypeShortname enabled tsStr
      return $ Core.Typed qt area [] (Core.Placeholder (placeholderRef', ts') exp')

     where
      optimizePlaceholderRef :: Slv.PlaceholderRef -> PostProcess Core.PlaceholderRef
      optimizePlaceholderRef phr = case phr of
        Slv.ClassRef cls ps call var -> do
          ps'  <- mapM optimizeClassRefPred ps
          cls' <- getClassShortname enabled cls
          return $ Core.ClassRef cls' ps' call var

        Slv.MethodRef cls mtd call -> do
          cls' <- getClassShortname enabled cls
          return $ Core.MethodRef cls' mtd call

      optimizeClassRefPred :: Slv.ClassRefPred -> PostProcess Core.ClassRefPred
      optimizeClassRefPred (Slv.CRPNode cls ts var ps) = do
        ps'  <- mapM optimizeClassRefPred ps
        cls' <- getClassShortname enabled cls
        let tsStr = Types.buildTypeStrForPlaceholder ts
        ts' <- getTypeShortname enabled tsStr
        return $ Core.CRPNode cls' ts' var ps'

    other ->
      error $ "not implemented: " <> ppShow other



instance Processable Slv.Typing Core.Typing where
  toCore enabled (Slv.Untyped area typing) = case typing of
    Slv.TRSingle name       -> return $ Core.Untyped area [] $ Core.TRSingle name

    Slv.TRComp name typings -> do
      typings' <- mapM (toCore enabled) typings
      return $ Core.Untyped area [] $ Core.TRComp name typings'

    Slv.TRArr left right -> do
      left'  <- toCore enabled left
      right' <- toCore enabled right
      return $ Core.Untyped area [] $ Core.TRArr left' right'

    Slv.TRRecord fields base -> do
      fields' <- mapM (toCore enabled) (snd <$> fields)
      base'   <- mapM (toCore enabled) base
      return $ Core.Untyped area [] $ Core.TRRecord fields' base'

    Slv.TRTuple typings -> do
      typings' <- mapM (toCore enabled) typings
      return $ Core.Untyped area [] $ Core.TRTuple typings'

    Slv.TRConstrained constraints typing -> do
      constraints' <- mapM (toCore enabled) constraints
      typing'      <- toCore enabled typing
      return $ Core.Untyped area [] $ Core.TRConstrained constraints' typing'

instance Processable Slv.ListItem Core.ListItem where
  toCore enabled (Slv.Typed qt area item) = case item of
    Slv.ListItem exp -> do
      exp' <- toCore enabled exp
      return $ Core.Typed qt area [] $ Core.ListItem exp'

    Slv.ListSpread exp -> do
      exp' <- toCore enabled exp
      return $ Core.Typed qt area [] $ Core.ListSpread exp'

instance Processable Slv.Field Core.Field where
  toCore enabled (Slv.Typed qt area item) = case item of
    Slv.Field (name, exp) -> do
      exp' <- toCore enabled exp
      return $ Core.Typed qt area [] $ Core.Field (name, exp')

    Slv.FieldSpread exp -> do
      exp' <- toCore enabled exp
      return $ Core.Typed qt area [] $ Core.FieldSpread exp'

instance Processable Slv.Is Core.Is where
  toCore enabled (Slv.Typed qt area (Slv.Is pat exp)) = do
    pat' <- toCore enabled pat
    exp' <- toCore enabled exp
    return $ Core.Typed qt area [] (Core.Is pat' exp')

instance Processable Slv.Pattern Core.Pattern where
  toCore enabled (Slv.Typed qt area pat) = case pat of
    Slv.PVar name       -> return $ Core.Typed qt area [] $ Core.PVar name

    Slv.PAny            -> return $ Core.Typed qt area [] Core.PAny

    Slv.PCon name pats -> do
      pats' <- mapM (toCore enabled) pats
      return $ Core.Typed qt area [] $ Core.PCon name pats'

    Slv.PNum    num  -> return $ Core.Typed qt area [] $ Core.PNum num

    Slv.PStr    str  -> return $ Core.Typed qt area [] $ Core.PStr str

    Slv.PChar   str  -> return $ Core.Typed qt area [] $ Core.PChar str

    Slv.PBool   boo  -> return $ Core.Typed qt area [] $ Core.PBool boo

    Slv.PRecord pats -> do
      pats' <- mapM (toCore enabled) pats
      return $ Core.Typed qt area [] $ Core.PRecord pats'

    Slv.PList pats -> do
      pats' <- mapM (toCore enabled) pats
      return $ Core.Typed qt area [] $ Core.PList pats'

    Slv.PTuple pats -> do
      pats' <- mapM (toCore enabled) pats
      return $ Core.Typed qt area [] $ Core.PTuple pats'

    Slv.PSpread pat -> do
      pat' <- toCore enabled pat
      return $ Core.Typed qt area [] $ Core.PSpread pat'

instance Processable Slv.TypeDecl Core.TypeDecl where
  toCore enabled (Slv.Untyped area typeDecl) = case typeDecl of
    adt@Slv.ADT{} -> do
      ctors <- mapM optimizeConstructors $ Slv.adtconstructors adt
      return $ Core.Untyped area [] $ Core.ADT { Core.adtname         = Slv.adtname adt
                                          , Core.adtparams       = Slv.adtparams adt
                                          , Core.adtconstructors = ctors
                                          , Core.adtexported     = Slv.adtexported adt
                                          }

    alias@Slv.Alias{} -> do
      aliastype <- toCore enabled $ Slv.aliastype alias
      return $ Core.Untyped area [] $ Core.Alias { Core.aliasname     = Slv.aliasname alias
                                                 , Core.aliasparams   = Slv.aliasparams alias
                                                 , Core.aliastype     = aliastype
                                                 , Core.aliasexported = Slv.aliasexported alias
                                                 }
   where
    optimizeConstructors :: Slv.Constructor -> PostProcess Core.Constructor
    optimizeConstructors (Slv.Untyped _ (Slv.Constructor name typings t)) = do
      typings' <- mapM (toCore enabled) typings
      return $ Core.Untyped area [] $ Core.Constructor name typings' t


instance Processable Slv.Interface Core.Interface where
  toCore enabled (Slv.Untyped area (Slv.Interface name constraints vars methods methodTypings)) = do
    name'          <- getClassShortname enabled name
    methodTypings' <- mapM (toCore enabled) methodTypings
    return $ Core.Untyped area [] $ Core.Interface name' constraints ((\(TV n _) -> n) <$> vars) methods methodTypings'

instance Processable Slv.Instance Core.Instance where
  toCore enabled (Slv.Untyped area (Slv.Instance interface constraints pred methods)) = do
    interface' <- getClassShortname enabled interface
    let typingStr = intercalate "_" (Types.getTypeHeadName <$> predTypes pred)
    typings' <- getTypeShortname enabled typingStr
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> toCore enabled exp) methods
    return $ Core.Untyped area [] $ Core.Instance interface' constraints typings' methods'

instance Processable Slv.Import Core.Import where
  toCore _ (Slv.Untyped area imp) = case imp of
    Slv.NamedImport names relPath absPath ->
      return $ Core.Untyped area [] $ Core.NamedImport (optimizeImportName <$> names) relPath absPath

    Slv.DefaultImport namespace relPath absPath ->
      return $ Core.Untyped area [] $ Core.DefaultImport (optimizeImportName namespace) relPath absPath


optimizeImportName :: Slv.Solved Slv.Name -> Core.Core Core.Name
optimizeImportName (Slv.Untyped area name) = Core.Untyped area [] name

instance Processable Slv.AST Core.AST where
  toCore enabled ast = do
    imports    <- mapM (toCore enabled) $ Slv.aimports ast
    exps       <- mapM (toCore enabled) $ Slv.aexps ast
    typeDecls  <- mapM (toCore enabled) $ Slv.atypedecls ast
    interfaces <- mapM (toCore enabled) $ Slv.ainterfaces ast
    instances  <- mapM (toCore enabled) $ Slv.ainstances ast

    return $ Core.AST
              { Core.aimports    = imports
              , Core.aexps       = exps
              , Core.atypedecls  = typeDecls
              , Core.ainterfaces = interfaces
              , Core.ainstances  = instances
              , Core.apath       = Slv.apath ast
              }


astToCore :: Bool -> Slv.AST -> Core.AST
astToCore enabled ast =
  MonadState.evalState (toCore enabled ast) initialOptimizationState


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
tableToCore :: Bool -> Slv.Table -> Core.Table
tableToCore enabled table =
  let core = mapM (toCore enabled) table
  in  MonadState.evalState core initialOptimizationState
