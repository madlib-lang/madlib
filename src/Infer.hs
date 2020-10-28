{-# LANGUAGE TupleSections #-}
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
import           Data.Foldable                  ( foldrM
                                                , Foldable(foldl')
                                                )
import           Type
import           Data.Char                      ( isLower )
import           Debug.Trace                    ( trace )
import qualified AST                           as AST
import           Text.Show.Pretty               ( ppShow )
import           Data.List                      ( union )


type Substitution = M.Map TVar Type


type Vars = M.Map String Scheme
type ADTs = M.Map String Type
type Typings = M.Map String Scheme
type Imports = M.Map (Name) Type


data Env
  = Env
    { envvars :: Vars
    , envadts :: ADTs
    , envtypings :: Typings
    , envimports :: Imports
    }
    deriving(Eq, Show)


data InferError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnificationError Type Type
  | ADTAlreadyDefined Type
  | UnknownType String
  | FieldNotExisting String
  | FieldNotInitialized String
  | ImportNotFound String String
  | FatalError
  | ASTHasNoPath
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
  apply s (  TRecord fields   ) = TRecord (apply s <$> fields)
  apply _ TAny                  = TAny

  ftv TCon{}              = S.empty
  ftv TAny                = S.empty
  ftv (TVar a           ) = S.singleton a
  ftv (t1    `TArr` t2  ) = ftv t1 `S.union` ftv t2
  ftv (TComp _      vars) = foldl' (\s v -> S.union s $ ftv v) S.empty vars
  ftv (TRecord fields   ) = foldl' (\s v -> S.union s $ ftv v) S.empty fields

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
    Nothing -> case M.lookup x $ envimports env of
      Nothing -> throwError $ UnboundVariable x
      Just s  -> do
        t <- instantiate $ Forall [] s
        return (M.empty, t)

    Just s -> do
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
s1 `compose` s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, s1]
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1, TRecord fields2) -> TRecord $ M.union fields1 fields2
    (t              , _              ) -> t


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

unify (TComp main vars) (TComp main' vars')
  | main == main' = do
    let z = zip vars vars'
    s2 <- unifyVars M.empty z
    return s2
  | otherwise = throwError
  $ UnificationError (TComp main vars) (TComp main' vars')

unify (TRecord fields) (TRecord fields')
  | M.difference fields fields' /= M.empty = throwError
  $ UnificationError (TRecord fields) (TRecord fields')
  | otherwise = do
    let types  = M.elems fields
        types' = M.elems fields'
        z      = zip types types'
    unifyVars M.empty z

unify (TVar a) t                 = bind a t
unify t        (TVar a)          = bind a t
unify (TCon a) (TCon b) | a == b = return M.empty
unify TAny _                     = return M.empty
unify _    TAny                  = return M.empty
unify t1   t2                    = throwError $ UnificationError t1 t2


unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify (apply s tp) (apply s tp')
  unifyVars s1 xs
unifyVars s [(tp, tp')] = unify (apply s tp) (apply s tp')
unifyVars s _           = return s


infer :: Env -> Exp -> Infer (Substitution, Type, Exp)
infer _ v@Var { ename = '.' : name } = do
  let s = Forall [TV "a"]
        $ TArr (TRecord (M.fromList [(name, TVar $ TV "a")])) (TVar $ TV "a")
  t <- instantiate s
  return (M.empty, t, v { etype = Just t })
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
  (s2, t2, e2) <- infer env earg
  -- TODO: That commented line caused inline functions in pipe to be wrongly typed.
  -- Not applying the substitution to the env seems to fix the problem. Nevertheless
  -- we should confirm that this is correct with the paper from Milner.
  -- (s2, t2, e2) <- infer (apply s1 env) earg
  s3           <- unify (apply s2 t1) (TArr t2 tv)
  let t = apply s3 tv
  return
    ( s3 `compose` s2 `compose` s1 --`compose` sMergedRecords
    , t
    , app { eabs  = e1
          , earg  = e2 { etype = Just $ apply s3 t2 }
          , etype = Just t
          }
    )

infer env ass@Assignment { eexp, ename } = case eexp of
  Abs{} -> do
    (s1, t1, e1) <- infer env eexp

    case M.lookup ename $ envtypings env of
      Just (Forall fv t2) -> do
        let bv = boundVariables t2
        it2 <- instantiate (Forall (fv <> bv) t2)
        s2  <- unify t1 it2
        return
          ( s2 `compose` s1
          , it2
          , ass { eexp = e1 { etype = Just it2 }, etype = Just it2 }
          )

      Nothing -> return (s1, t1, ass { eexp = e1, etype = Just t1 })

  _ -> do
    (s1, t1, e1) <- infer env eexp
    return (s1, t1, ass { eexp = e1, etype = Just t1 })

infer _ l@LInt{} = return (M.empty, TCon CNum, l { etype = Just $ TCon CNum })
infer _ l@LStr{} =
  return (M.empty, TCon CString, l { etype = Just $ TCon CString })
infer _ l@LBool{} =
  return (M.empty, TCon CBool, l { etype = Just $ TCon CBool })

-- TODO: Needs to handle quantified variables ?
-- TODO: Add TComp
-- TODO: Add TArr
-- So that we can write a type :
-- :: (a -> b) -> List a -> List b
infer _ te@TypedExp { etyping } = do
  t <- typingsToType etyping
  return (M.empty, t, te { etype = Just t })

infer env rec@Record { erfields } = do
  inferred <- mapM (infer env) erfields
  let inferredFields = M.map trd inferred
      recordType     = TRecord $ M.map mid inferred
  return
    ( M.empty
    , recordType
    , rec { etype = Just recordType, erfields = inferredFields }
    )

infer _ lc@ListConstructor { eelems = [] } =
  let t = TComp "List" [TVar $ TV "a"]
  in  return (M.empty, t, lc { etype = Just t })

infer env lc@ListConstructor { eelems } = do
  inferred <- mapM (infer env) eelems
  let (s1, t1, e1) = head inferred
  let t            = TComp "List" [t1]
  s <- unifyElems t1 (mid <$> inferred)

  return (s, t, lc { etype = Just t })

infer env sw@Switch { ecases, eexp } = do
  (se, te, ee)  <- infer env eexp

  inferredCases <- mapM (inferCase env te) ecases
  let casesSubstitution = foldr1 compose $ se : (beg <$> inferredCases)
  let casesTypes        = mid <$> inferredCases
  let cases             = trd <$> inferredCases

  let typeMatrix        = (\c -> (c, casesTypes)) <$> casesTypes
  s <-
    foldr1 compose
      <$> mapM (\(t, ts) -> unifyElems (apply casesSubstitution t) ts)
               typeMatrix

  let updatedCases =
        (\(t, e) -> e { casetype = Just $ apply s t }) <$> zip casesTypes cases

  let (TArr _ switchType) = (apply s . head) casesTypes

  return
    ( s
    , switchType
    , sw { ecases = updatedCases
         , etype  = Just switchType
         , eexp   = ee { etype = Just $ apply s te }
         }
    )
 where
  inferCase :: Env -> Type -> Case -> Infer (Substitution, Type, Case)
  inferCase e tinput c@Case { casepattern, caseexp } = do
    tp           <- buildPatternType e casepattern
    tu           <- flip apply tp <$> unify tp tinput
    e'           <- generateCaseEnv tu e casepattern

    (se, te, ee) <- infer e' caseexp
    let tarr  = TArr (apply se tu) te
    let tarr' = TArr (apply se tinput) te
    su <- unify tarr tarr'

    let sf = su `compose` se

    return
      ( sf
      , tarr
      , c { casetype = Just $ apply sf tarr
          , caseexp  = ee { etype = Just $ apply sf te }
          }
      )

  buildPatternType :: Env -> Pattern -> Infer Type
  buildPatternType e@Env { envvars } pattern = case pattern of
    PVar  v        -> return $ TVar $ TV v

    PCon  "String" -> return $ TCon CString
    PCon  "Bool"   -> return $ TCon CBool
    PCon  "Num"    -> return $ TCon CNum

    PStr  _        -> return $ TCon CString
    PBool _        -> return $ TCon CBool
    PNum  _        -> return $ TCon CNum

    PAny           -> return $ TVar $ TV "a"

    PRecord fields -> TRecord . M.fromList <$> mapM
      (\(k, v) -> (k, ) <$> buildPatternType e v)
      (M.toList fields)

    PCtor n as -> do
      (Forall fv ctor) <- case M.lookup n envvars of
        Just x  -> return x
        Nothing -> throwError $ UnknownType n

      let rt = arrowReturnType ctor
      ctor'  <- argPatternsToArrowType rt as
      ctor'' <- instantiate $ Forall fv ctor
      s      <- unify ctor' ctor''
      return $ apply s rt
     where
      argPatternsToArrowType :: Type -> [Pattern] -> Infer Type
      argPatternsToArrowType rt (f : xs) = do
        l <- buildPatternType e f
        r <- argPatternsToArrowType rt xs
        return $ TArr l r
      argPatternsToArrowType _  [x] = buildPatternType e x
      argPatternsToArrowType rt []  = return rt
    _ -> return $ TVar $ TV "x"


  generateCaseEnv :: Type -> Env -> Pattern -> Infer Env
  generateCaseEnv t e@Env { envvars } pattern = case (pattern, t) of
    (PVar v, t') -> do
      return $ extendVars e (v, Forall [] t')

    (PRecord fields, TRecord fields') ->
      let allFields = zip (M.elems fields) (M.elems fields')
      in  foldrM (\(p, t) e' -> generateCaseEnv t e' p) e allFields

    (PCtor cname as, t) -> do
      ctor <- findConstructor cname

      case (ctor, as) of
        ((TArr a _), [a']) -> do
          generateCaseEnv a e a'

        (TArr a (TArr b _), [a', b']) -> do
          e1 <- generateCaseEnv a e a'
          generateCaseEnv b e1 b'

        (TArr a (TArr b (TArr c _)), [a', b', c']) -> do
          e1 <- generateCaseEnv a e a'
          e2 <- generateCaseEnv b e1 b'
          generateCaseEnv c e2 c'

        _ -> return e

    _ -> return e

   where
    findConstructor :: String -> Infer Type
    findConstructor cname = case M.lookup cname envvars of
      Just (Forall _ t) -> return t

      Nothing           -> throwError $ UnknownType cname




infer _ e@JSExp { etype = Just t } = return (M.empty, t, e)

unifyElems :: Type -> [Type] -> Infer Substitution
unifyElems _ []        = return M.empty
unifyElems t [t'     ] = unify t t'
unifyElems t (t' : xs) = do
  s1 <- unify t t'
  s2 <- unifyElems t xs
  return $ s1 `compose` s2

boundVariables :: Type -> [TVar]
boundVariables (TVar t    ) = [t]
boundVariables (TComp _ xs) = concat $ boundVariables <$> xs
boundVariables (TArr  t t') = boundVariables t `union` boundVariables t'
boundVariables _            = []


arrowReturnType :: Type -> Type
arrowReturnType (TArr _ (TArr y x)) = arrowReturnType (TArr y x)
arrowReturnType (TArr _ x         ) = x
arrowReturnType x                   = x


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

        TypedExp { eexp = Var { ename } } ->
          extendTypings env (ename, Forall [] $ mid r)
        _ -> env
  (\n -> trd r : n) <$> inferExps env' xs


trd :: (a, b, c) -> c
trd (_, _, x) = x

mid :: (a, b, c) -> b
mid (_, b, _) = b

beg :: (a, b, c) -> a
beg (a, _, _) = a


-- TODO: Make it call inferAST so that inferAST can return an (Infer TBD)
-- Well, or just adapt it somehow
runInfer :: Env -> AST -> Either InferError AST
runInfer env ast = (\e -> ast { aexps = e }) <$> inferredExps
 where
  inferredExps = fst
    <$> runExcept (runStateT (inferExps env $ aexps ast) Unique { count = 0 })

-- TODO: Missing recursion
inferAST :: AST.ASTTable -> AST -> Infer AST.ASTTable
inferAST table ast@AST { aimports } = do
  env <- buildInitialEnv ast

  let importPaths = ipath <$> aimports
      asts        = mapM (AST.findAST table)
                         (("fixtures/" ++) . (++ ".mad") <$> importPaths)

  inferredASTs <- case asts of
    Right x                         -> inferASTs table x

    Left  (AST.ImportNotFound fp _) -> throwError $ ImportNotFound fp ""
    Left  (AST.ASTNotFound fp     ) -> throwError $ ImportNotFound fp ""

  exportedExps <- M.fromList . join <$> mapM exportedExps (M.elems inferredASTs)

  let exportedTypes = mapM etype exportedExps
  envWithImports <- case exportedTypes of
    Just et -> return env { envimports = et }
    Nothing -> throwError $ ImportNotFound "" ""

  inferredAST <- inferASTExps envWithImports ast

  case apath inferredAST of
    Just fp -> return $ M.union inferredASTs $ M.fromList [(fp, inferredAST)]
    Nothing -> throwError ASTHasNoPath


exportedExps :: AST -> Infer [(Name, Exp)]
exportedExps AST { aexps, apath } = case apath of
  Just p  -> mapM (bundleExports p) $ filter eexported aexps

  Nothing -> throwError ASTHasNoPath
  where bundleExports _ exp@Assignment { ename } = return (ename, exp)


inferASTs :: AST.ASTTable -> [AST] -> Infer AST.ASTTable
inferASTs _     []       = return M.empty
inferASTs table [l     ] = inferAST table l
inferASTs table (a : xs) = do
  nexts <- inferASTs table xs
  M.union nexts <$> inferAST table a


inferASTExps :: Env -> AST -> Infer AST
inferASTExps env ast@AST { aexps } = do
  inferredExps <- inferExps env aexps
  return ast { aexps = inferredExps }


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envvars = M.insert x s $ envvars env }


extendTypings :: Env -> (String, Scheme) -> Env
extendTypings env (x, s) = env { envtypings = M.insert x s $ envtypings env }


findImportType :: Env -> Name -> Infer Type
findImportType env name = case M.lookup name (envimports env) of
  Just x  -> return x
  Nothing -> throwError $ ImportNotFound "" name


initialEnv :: Env
initialEnv = Env
  { envvars    = M.fromList
    [ ( "==="
      , Forall [TV "a"] $ TVar (TV "a") `TArr` TVar (TV "a") `TArr` TCon CBool
      )
    , ("+", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ("-", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ("*", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ("/", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ( "|>"
      , Forall [TV "a", TV "b"]
      $      (TVar $ TV "a")
      `TArr` ((TVar $ TV "a") `TArr` (TVar $ TV "b"))
      `TArr` (TVar $ TV "b")
      )
    , ( "ifElse"
      , Forall [TV "a"]
      $      TCon CBool
      `TArr` (TVar $ TV "a")
      `TArr` (TVar $ TV "a")
      `TArr` (TVar $ TV "a")
      )
    , ( "asList"
      , Forall [TV "a"] $ TArr (TVar $ TV "a") $ TComp "List" [TVar $ TV "a"]
      )
    ]
  , envadts    = M.empty
  , envtypings = M.empty
  , envimports = M.empty
  }


-- TODO: Should we build imported names here ?
buildInitialEnv :: AST -> Infer Env
buildInitialEnv AST { aadts } = do
  tadts <- buildADTTypes aadts
  vars  <- resolveADTs tadts aadts
  let allVars = M.union (envvars initialEnv) vars
  return Env { envvars    = allVars
             , envadts    = tadts
             , envtypings = M.empty
             , envimports = M.empty
             }


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
  Just t  -> throwError $ ADTAlreadyDefined t
  Nothing -> return (adtname, TComp adtname (TVar . TV <$> adtparams))


resolveADTs :: ADTs -> [ADT] -> Infer Vars
resolveADTs tadts adts = mergeVars <$> mapM (resolveADT tadts) adts
 where
  mergeVars []   = M.empty
  mergeVars vars = foldr1 M.union vars


resolveADT :: ADTs -> ADT -> Infer Vars
resolveADT tadts ADT { adtname, adtconstructors, adtparams } =
  foldr1 M.union
    <$> mapM (resolveADTConstructor tadts adtname adtparams) adtconstructors


-- TODO: Verify that Constructors aren't already in the global space or else throw a name clash error
resolveADTConstructor :: ADTs -> Name -> [Name] -> ADTConstructor -> Infer Vars
resolveADTConstructor tadts n params ADTConstructor { adtcname, adtcargs } = do
  let t = buildADTConstructorReturnType n params
  case adtcargs of
    Just cargs -> do
      t' <- mapM (argToType tadts n params) cargs
      let ctype = foldr1 (TArr) (t' <> [t])
      return $ M.fromList [(adtcname, Forall (TV <$> params) ctype)]
    Nothing -> return $ M.fromList [(adtcname, Forall (TV <$> params) t)]

-- TODO: This should probably be merged with typingToType somehow
argToType :: ADTs -> Name -> [Name] -> TypeRef -> Infer Type
argToType tadts _ params (TRSingle n)
  | n == "Num" = return $ TCon CNum
  | n == "Bool" = return $ TCon CBool
  | n == "String" = return $ TCon CString
  | isLower (head n) && (n `elem` params) = return $ TVar $ TV n
  | isLower (head n) = throwError $ UnboundVariable n
  | otherwise = case M.lookup n tadts of
    Just a  -> return a
    -- If the lookup gives a Nothing, it should most likely be an undefined type error ?
    Nothing -> return $ TCon $ CUserDef n
argToType tadts name params (TRComp tname targs) = case M.lookup tname tadts of
  -- TODO: Verify the length of tparams and make sure it matches the one of targs ! otherwise
  -- we have a type application error.
  Just (TComp n _) -> TComp n <$> mapM (argToType tadts name params) targs
  Nothing          -> return $ TCon $ CUserDef name
argToType tadts name params (TRArr l r) = do
  l' <- (argToType tadts name params l)
  r' <- (argToType tadts name params r)
  return $ TArr l' r'
argToType tadts name params (TRRecord f) = do
  f' <- mapM (argToType tadts name params) f
  return $ TRecord f'



typingsToType :: TypeRef -> Infer Type
typingsToType (TRSingle t) | t == "Num"    = return $ TCon CNum
                           | t == "Bool"   = return $ TCon CBool
                           | t == "String" = return $ TCon CString
                           | t == "Void"   = return $ TCon CVoid
                           | otherwise     = return $ TVar $ TV t
typingsToType (TRComp t ts) = do
  params <- mapM typingsToType ts
  return $ TComp t params
typingsToType (TRArr l r) = do
  l' <- typingsToType l
  r' <- typingsToType r
  return $ TArr l' r'
typingsToType (TRRecord f) = do
  f' <- mapM typingsToType f
  return $ TRecord f'



buildADTConstructorReturnType :: Name -> [Name] -> Type
buildADTConstructorReturnType tname tparams =
  TComp tname $ TVar . TV <$> tparams
