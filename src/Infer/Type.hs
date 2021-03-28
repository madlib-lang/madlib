{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Type where

import qualified Data.Map                      as M
import           AST.Source                     ( Exp )
import           Data.List                      ( nub
                                                , union
                                                )


data TVar = TV Id Kind
  deriving (Show, Eq, Ord)

-- TODO: Add FilePath from origin module
data TCon = TC Id Kind
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar          -- Variable type
  | TCon TCon FilePath -- Constructor type - FilePath of where that type is defined
  | TGen Int
  | TApp Type Type              -- Arrow type
  | TRecord (M.Map Id Type) Bool -- Record type: Bool means open or closed
  | TAlias FilePath Id [TVar] Type -- Aliases, filepath of definition module, name, params, type it aliases
  deriving (Show, Eq, Ord)

infixr `TApp`


tNumber :: Type
tNumber = TCon (TC "Number" Star) "prelude"

tBool :: Type
tBool = TCon (TC "Boolean" Star) "prelude"

tStr :: Type
tStr = TCon (TC "String" Star) "prelude"

tUnit :: Type
tUnit = TCon (TC "()" Star) "prelude"

tList :: Type
tList = TApp (TCon (TC "List" (Kfun Star Star)) "prelude") (TVar (TV "a" Star))

tTuple2 :: Type
tTuple2 = TCon (TC "(,)" (Kfun Star (Kfun Star Star))) "prelude"

tTuple3 :: Type
tTuple3 = TCon (TC "(,,)" (Kfun Star (Kfun Star (Kfun Star Star)))) "prelude"

tTuple4 :: Type
tTuple4 = TCon (TC "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))) "prelude"

tTuple5 :: Type
tTuple5 = TCon (TC "(,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))) "prelude"

tTuple6 :: Type
tTuple6 = TCon (TC "(,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))) "prelude"

tTuple7 :: Type
tTuple7 = TCon (TC "(,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))) "prelude"

tArrow :: Type
tArrow = TCon (TC "(->)" (Kfun Star (Kfun Star Star))) "prelude"

getTupleCtor :: Int -> Type
getTupleCtor n = case n of
  2 -> tTuple2
  3 -> tTuple3
  4 -> tTuple4
  5 -> tTuple5
  6 -> tTuple6
  7 -> tTuple7

infixr      4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TApp (TApp tArrow a) b


predClass :: Pred -> Id
predClass (IsIn i _) = i


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


class HasKind t where
  kind :: t -> Kind
instance HasKind TVar where
  kind (TV _ k) = k
instance HasKind TCon where
  kind (TC _ k) = k
instance HasKind Type where
  kind (TCon tc _) = kind tc
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
  TCon _ _      -> Nothing
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
  TCon _ _    -> t
  TApp    l r -> getConstructorCon l
  TRecord _ _ -> t
  _           -> t

closeRecords :: Type -> Type
closeRecords t = case t of
  TVar _           -> t
  TCon _ _         -> t
  TApp l r         -> TApp (closeRecords l) (closeRecords r)
  TGen _           -> t
  TRecord fields _ -> TRecord fields False

mergeRecords :: Type -> Type -> Type
mergeRecords t1 t2 = case (t1, t2) of
  (TRecord fields1 True, TRecord fields2 open2) -> TRecord (M.union fields1 fields2) True

  (TApp l r, TApp l' r') -> TApp (mergeRecords l l') (mergeRecords r r')

  _ -> t1

