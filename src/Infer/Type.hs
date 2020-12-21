{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Type where

import qualified Data.Map                      as M
import           AST.Source                     ( Exp )
import           Text.Show.Pretty               ( ppShow )
import           Debug.Trace                    ( trace )
import qualified Data.Set                      as S
import           Data.List                      ( nub
                                                , union
                                                )


type Vars = M.Map String Scheme
type Interfaces = M.Map Id Interface
type Methods = M.Map String Scheme
type TypeDecls = M.Map String Type
type Constructors = M.Map String (String, Scheme)



data Interface = Interface [TVar] [Pred] [Instance] deriving(Eq, Show)

newtype Instance = Instance (Qual Pred) deriving(Eq, Show)


data Env
  = Env
    { envvars         :: Vars
    , envtypes        :: TypeDecls
    , envinterfaces   :: Interfaces
    , envmethods      :: Methods
    , envcurrentpath  :: FilePath
    -- , envconstructors :: Constructors
    }
    deriving(Eq, Show)


data TVar = TV Id Kind
  deriving (Show, Eq, Ord)

data TCon = TC Id Kind
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar          -- Variable type
  | TCon TCon                   -- Constructor type
  | TGen Int
  | TApp Type Type              -- Arrow type
  | TRecord (M.Map Id Type) Bool -- Record type: Bool means open or closed
  | TAlias FilePath Id [TVar] Type -- Aliases, filepath of definition module, name, params, type it aliases
  deriving (Show, Eq, Ord)

infixr `TApp`


tNumber :: Type
tNumber = TCon $ TC "Number" Star

tBool :: Type
tBool = TCon $ TC "Boolean" Star

tStr :: Type
tStr = TCon $ TC "String" Star

tUnit :: Type
tUnit = TCon $ TC "()" Star

tList :: Type
tList = TApp (TCon $ TC "List" (Kfun Star Star)) (TVar (TV "a" Star))

tTuple2 :: Type
tTuple2 = TCon $ TC "(,)" (Kfun Star (Kfun Star Star))

tTuple3 :: Type
tTuple3 = TCon $ TC "(,,)" (Kfun Star (Kfun Star (Kfun Star Star)))

tTuple4 :: Type
tTuple4 =
  TCon $ TC "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))

tArrow :: Type
tArrow = TCon $ TC "(->)" (Kfun Star (Kfun Star Star))

getTupleCtor :: Int -> Type
getTupleCtor n = case n of
  2 -> tTuple2
  3 -> tTuple3
  4 -> tTuple4

infixr      4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TApp (TApp tArrow a) b


predClass :: Pred -> Id
predClass (IsIn i _) = i

predType :: Pred -> Type
predType (IsIn _ [t     ]) = t
predType (IsIn _ (t : ts)) = t

predTypes :: Pred -> [Type]
predTypes (IsIn _ ts) = ts


type Id = String

data Kind  = Star | Kfun Kind Kind
             deriving (Eq, Show, Ord)

data Pred   = IsIn Id [Type]
              deriving (Eq, Show, Ord)

data Qual t = [Pred] :=> t
              deriving (Eq, Show, Ord)

data Scheme = Forall [Kind] (Qual Type)
              deriving (Eq, Show, Ord)


type Substitution = M.Map TVar Type

nullSubst :: Substitution
nullSubst = M.empty


qualType :: Qual t -> t
qualType (_ :=> t) = t


class HasKind t where
  kind :: t -> Kind
instance HasKind TVar where
  kind (TV _ k) = k
instance HasKind TCon where
  kind (TC _ k) = k
instance HasKind Type where
  kind (TCon tc ) = kind tc
  kind (TVar u  ) = kind u
  kind (TApp t _) = case kind t of
    (Kfun _ k) -> k
    k          -> k
  kind _ = Star

buildKind :: Int -> Kind
buildKind n | n > 0     = Kfun Star $ buildKind (n - 1)
            | otherwise = Star


searchVarInType :: Id -> Type -> Maybe Type
searchVarInType id t = case t of
  TVar (TV n _) -> if n == id then Just t else Nothing
  TCon _        -> Nothing
  TApp l r ->
    let l' = searchVarInType id l
        r' = searchVarInType id r
    in  case (l', r') of
          (Just x, _     ) -> Just x
          (_     , Just x) -> Just x
          _                -> Nothing


isTVar :: Type -> Bool
isTVar t = case t of
  TVar _ -> True
  _      -> False


collectQualTypeVars :: Qual Type -> [TVar]
collectQualTypeVars (ps :=> t) =
  S.toList $ S.fromList $ collectVars t ++ concat (collectPredVars <$> ps)


collectVars :: Type -> [TVar]
collectVars t = case t of
  TVar tv      -> [tv]
  TApp    l  r -> collectVars l `union` collectVars r
  TRecord fs _ -> nub $ concat $ collectVars <$> M.elems fs
  _            -> []


collectPredVars :: Pred -> [TVar]
collectPredVars (IsIn _ ts) = nub $ concat $ collectVars <$> ts


getConstructorCon :: Type -> Type
getConstructorCon t = case t of
  TCon _      -> t
  TApp    l r -> getConstructorCon l
  TRecord _ _ -> t
  _           -> t
