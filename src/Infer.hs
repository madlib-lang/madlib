{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Infer where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.Except
import           Control.Monad.State
import           Grammar
import           Data.Foldable                  ( Foldable(foldl') )
import           Type
import           Data.Char                      ( isLower )


-- TODO: Add scoped names ( M.Map, Internal.buildList ( from this grammar : [args] ), ...)

type Substitution = M.Map TVar Type

type Vars = M.Map String Scheme
type ADTs = M.Map String Type
type Typings = M.Map String Scheme

data Env
  = Env
    { envvars :: Vars
    , envadts :: ADTs
    , envtypings :: Typings
    }
    deriving(Eq, Show)

data InferError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnificationError Type Type
  | ADTAlreadyDefined Type
  deriving (Show, Eq, Ord)

newtype Unique = Unique { count :: Int }
  deriving (Show, Eq, Ord)

type Infer a = forall m . (MonadError InferError m, MonadState Unique m) => m a


class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> S.Set TVar

instance Substitutable Type where
  apply _ (  TCon a           ) = TCon a
  apply s t@(TVar a           ) = M.findWithDefault t a s
  apply s (  t1    `TArr` t2  ) = apply s t1 `TArr` apply s t2
  apply s (  TComp main   vars) = TComp main (apply s <$> vars)

  ftv TCon{}              = S.empty
  ftv (TVar a           ) = S.singleton a
  ftv (t1    `TArr` t2  ) = ftv t1 `S.union` ftv t2
  ftv (TComp _      vars) = foldl' (\s v -> S.union s $ ftv v) S.empty vars

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr M.delete s as
  ftv (Forall as t) = S.difference (ftv t) (S.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable Env where
  apply s env = env { envvars = M.map (apply s) $ envvars env }
  ftv env = ftv $ M.elems $ envvars env


lookupVar :: Env -> String -> Infer (Substitution, Type)
lookupVar env x = do
  case M.lookup x $ envvars env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> do
      t <- instantiate s
      return (M.empty, t)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

newTVar :: Infer Type
newTVar = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ TV (letters !! count s)


instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const newTVar) as
  let s = M.fromList $ zip as as'
  return $ apply s t


compose :: Substitution -> Substitution -> Substitution
s1 `compose` s2 = M.map (apply s1) $ M.union s2 s1


occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = S.member a $ ftv t


bind :: TVar -> Type -> Infer Substitution
bind a t | t == TVar a     = return M.empty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ M.singleton a t


unify :: Type -> Type -> Infer Substitution
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TComp main vars) (TComp main' vars') = do
  s1 <- unify (TCon main) (TCon main')
  let z = zip vars vars'
  s2 <- unifyVars s1 z
  return (s2 `compose` s1)
 where
  unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
  unifyVars s ((tp, tp') : xs) = do
    s1 <- unify (apply s tp) (apply s tp')
    unifyVars s1 xs
  unifyVars s [(tp, tp')] = unify (apply s tp) (apply s tp')
  unifyVars s _           = return s

unify (TVar a) t                 = bind a t
unify t        (TVar a)          = bind a t
unify (TCon a) (TCon b) | a == b = return M.empty
unify t1 t2                      = throwError $ UnificationError t1 t2


infer :: Env -> Exp -> Infer (Substitution, Type, Exp)
infer env v@Var { ename } =
  (\(s, t) -> (s, t, v { etype = Just t })) <$> lookupVar env ename

infer env abs@Abs { eparam, ebody } = do
  tv <- newTVar
  let env' = extendVars env (eparam, Forall [] tv)
  (s1, t1, e) <- infer env' ebody
  let t = apply s1 tv `TArr` t1
  return (s1, t, abs { ebody = e, etype = Just t })

infer env app@App { eabs, earg } = do
  tv           <- newTVar
  (s1, t1, e1) <- infer env eabs
  (s2, t2, e2) <- infer (apply s1 env) earg
  s3           <- unify (apply s2 t1) (TArr t2 tv)
  let t = apply s3 tv
  return
    ( s3 `compose` s2 `compose` s1
    , t
    , app { eabs = e1, earg = e2, etype = Just t }
    )

infer env ass@Assignment { eexp, ename } = case eexp of
  Abs{} -> do
    (s1, t1, e1) <- infer env eexp
    s2           <- case M.lookup ename $ envtypings env of
      Just t2 -> instantiate t2 >>= unify t1
      Nothing -> return s1
    return (s2 `compose` s1, t1, ass { eexp = e1, etype = Just t1 })
  _ -> do
    (s1, t1, e1) <- infer env eexp
    return (s1, t1, ass { eexp = e1, etype = Just t1 })

infer _ l@LInt{} = return (M.empty, TCon CNum, l { etype = Just $ TCon CNum })
infer _ l@LStr{} =
  return (M.empty, TCon CString, l { etype = Just $ TCon CString })
infer _ l@LBool{} =
  return (M.empty, TCon CBool, l { etype = Just $ TCon CBool })

-- TODO: Needs to handle quantified variables ?
infer _ te@TypedExp { etyping } = return (M.empty, t, te { etype = Just t })
  where t = typingsToType etyping


typingsToType :: [Typing] -> Type
typingsToType [t     ] = typingToType t
typingsToType (t : xs) = TArr (typingToType t) (typingsToType xs)

typingToType :: Typing -> Type
typingToType (Typing t) | t == "Num"    = TCon CNum
                        | t == "Bool"   = TCon CBool
                        | t == "String" = TCon CString
                        | otherwise     = TVar $ TV t

-- TODO: If we want to allow multiple Exp per abstraction, we'll have to move this and
-- make it work at any depth of the AST.
inferExps :: Env -> [Exp] -> Infer [Exp]
inferExps _   []       = return []
inferExps env [exp   ] = (: []) . trd <$> infer env exp
inferExps env (e : xs) = do
  r <- infer env e
  let env' = case e of
        -- TODO: We need to add a case where that name is already in the env.
        -- Reassigning a name should not be allowed.
        Assignment { ename } -> extendVars env (ename, Forall [] $ mid r)
        -- TODO: This should push in a separate part of the ENV ( envtypings ),
        -- so that we can validate the assignment expression it belongs to.
        TypedExp { eexp = Var { ename } } ->
          extendTypings env (ename, Forall [] $ mid r)
        _ -> env
  (\n -> trd r : n) <$> inferExps env' xs

trd :: (a, b, c) -> c
trd (_, _, x) = x

mid :: (a, b, c) -> b
mid (_, b, _) = b

runInfer :: Env -> AST -> Either InferError AST
runInfer env ast = (\e -> ast { aexps = e }) <$> inferredExps
 where
  inferredExps = fst
    <$> runExcept (runStateT (inferExps env $ aexps ast) Unique { count = 0 })



extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envvars = M.insert x s $ envvars env }

extendTypings :: Env -> (String, Scheme) -> Env
extendTypings env (x, s) = env { envtypings = M.insert x s $ envtypings env }

initialEnv :: Env
initialEnv = Env
  { envvars    = M.fromList
                   [ ( "==="
                     , Forall [TV "a"]
                     $      TVar (TV "a")
                     `TArr` TVar (TV "a")
                     `TArr` TCon CBool
                     )
                   , ("+", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
                   , ("-", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
                   , ("*", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
                   , ("/", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
                   , ( "asList"
                     , Forall [TV "a"] $ TArr (TVar $ TV "a") $ TComp
                       (CUserDef "List")
                       [TVar $ TV "a"]
                     )
                   ]
  , envadts    = M.empty
  , envtypings = M.empty
  }


buildInitialEnv :: AST -> Infer Env
buildInitialEnv AST { aadts } = do
  tadts <- buildADTTypes aadts
  vars  <- resolveADTs tadts aadts
  let allVars = M.union (envvars initialEnv) vars
  return Env { envvars = allVars, envadts = tadts, envtypings = M.empty }

buildADTTypes :: [ADT] -> Infer ADTs
buildADTTypes = buildADTTypes' M.empty

buildADTTypes' :: ADTs -> [ADT] -> Infer ADTs
buildADTTypes' _    []    = return M.empty
buildADTTypes' adts [adt] = do
  (k, v) <- buildADTType adts adt
  return $ M.singleton k v
buildADTTypes' adts (adt : xs) = do
  a    <- buildADTTypes' adts [adt]
  next <- buildADTTypes' (M.union a adts) xs
  return $ M.union a next

buildADTType :: ADTs -> ADT -> Infer (String, Type)
buildADTType adts ADT { adtname, adtparams } = case M.lookup adtname adts of
  Just t -> throwError $ ADTAlreadyDefined t
  Nothing ->
    return (adtname, TComp (CUserDef adtname) (TVar . TV <$> adtparams))

resolveADTs :: ADTs -> [ADT] -> Infer Vars
resolveADTs tadts adts = mergeVars <$> mapM (resolveADT tadts) adts
  where
    mergeVars []   = M.empty
    mergeVars vars = foldr1 M.union vars

resolveADT :: ADTs -> ADT -> Infer Vars
resolveADT tadts ADT { adtname, adtconstructors, adtparams } =
  foldr1 M.union <$> mapM (resolveADTConstructor tadts adtname adtparams) adtconstructors

-- TODO: Update the return type of it so that we can also generate all functions to access fields
resolveADTConstructor
  :: ADTs -> Name -> [Name] -> ADTConstructor -> Infer Vars
resolveADTConstructor _ n params ADTConstructor { adtcname, adtcargs = [] } =
  return $
    M.fromList [(adtcname, Forall (TV <$> params) $ buildADTConstructorReturnType n params)]

resolveADTConstructor tadts n params ADTRecordConstructor { adtcname, adtcfields } = do
  types <- mapM (argToType tadts n params) $ adtrcftype <$> adtcfields
  let recordType = buildADTConstructorReturnType n params
      allTypes   = types <> [recordType]
      typeArray  = foldr1 TArr allTypes
  fieldAccessors <- mapM (fieldToAccessorType recordType) adtcfields
  return $ M.fromList $ (adtcname, Forall (TV <$> params) typeArray) : fieldAccessors
  where
    makeFieldAccessorType fieldName recordType fieldType = (fieldName, Forall (TV <$> params) $ TArr recordType fieldType)
    fieldToAccessorType recordType field = makeFieldAccessorType (adtrcfname field) recordType <$> argToType tadts n params (adtrcftype field)

resolveADTConstructor tadts n params ADTConstructor { adtcname, adtcargs } = do
  types <- mapM (argToType tadts n params) adtcargs
  let allTypes  = types <> [buildADTConstructorReturnType n params]
      typeArray = foldr1 TArr allTypes
  return $ M.fromList [(adtcname, Forall (TV <$> params) typeArray)]

-- TODO: This should probably be merged with typingToType somehow
argToType :: ADTs -> Name -> [Name] -> TypeRef -> Infer Type
argToType tadts _ params (TRSingle n)
  | n == "String" = return $ TCon CString
  | n == "Bool" = return $ TCon CBool
  | n == "Num" = return $ TCon CNum
  | isLower (head n) && (n `elem` params) = return $ TVar $ TV n
  | isLower (head n) = throwError $ UnboundVariable n
  | otherwise = case M.lookup n tadts of
    Just a  -> return a
    -- If the lookup gives a Nothing, it should most likely be an undefined type error ?
    Nothing -> return $ TCon $ CUserDef n
argToType tadts name params (TRComp ((TRSingle tname) : targs)) =
  case M.lookup tname tadts of
  -- TODO: Verify the length of tparams and make sure it matches the one of targs ! otherwise
  -- we have a type application error.
    Just (TComp n tparams) -> TComp n <$> mapM (argToType tadts name params) targs
    Nothing                -> return $ TCon $ CUserDef name

buildADTConstructorReturnType :: Name -> [Name] -> Type
buildADTConstructorReturnType tname tparams =
  TComp (CUserDef tname) $ TVar . TV <$> tparams
