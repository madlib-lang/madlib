{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Type where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           AST.Source                     ( Exp )
import           Data.List                      ( nub
                                                , union
                                                )
import           Explain.Location


data TVar = TV Id Kind
  deriving (Show, Eq, Ord)

data TCon = TC Id Kind
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar          -- Variable type
  | TCon TCon FilePath -- Constructor type - FilePath of where that type is defined
  | TGen Int
  | TApp Type Type              -- Arrow type
  | TRecord (M.Map Id Type) (Maybe Type) -- Maybe Type is the extended record type, most likely a type variable
  | TAlias FilePath Id [TVar] Type -- Aliases, filepath of definition module, name, params, type it aliases
  deriving (Show, Eq, Ord)

infixr `TApp`

getTConId :: TCon -> Id
getTConId (TC id _) = id

getTVarId :: TVar -> Id
getTVarId (TV id _) = id

getTV :: Type -> TVar
getTV t = case t of
  TVar tv ->
    tv

  _ ->
    undefined


tVar :: String -> Type
tVar v = TVar (TV v Star)

tNumber :: Type
tNumber = TVar (TV "a" Star)

tFloat :: Type
tFloat = TCon (TC "Float" Star) "prelude"

tInteger :: Type
tInteger = TCon (TC "Integer" Star) "prelude"

tByte :: Type
tByte = TCon (TC "Byte" Star) "prelude"

qNumber :: Qual Type
qNumber = [IsIn "Number" [TVar (TV "a" Star)] Nothing] :=> TVar (TV "a" Star)

tBool :: Type
tBool = TCon (TC "Boolean" Star) "prelude"

tStr :: Type
tStr = TCon (TC "String" Star) "prelude"

tChar :: Type
tChar = TCon (TC "Char" Star) "prelude"

tUnit :: Type
tUnit = TCon (TC "{}" Star) "prelude"

tList :: Type
tList = tListOf (TVar (TV "a" Star))

tListOf :: Type -> Type
tListOf = TApp (TCon (TC "List" (Kfun Star Star)) "prelude")

tDictionary :: Type
tDictionary = TApp (TApp (TCon (TC "Dictionary" (Kfun Star (Kfun Star Star))) "prelude") (TVar (TV "a" Star))) (TVar (TV "b" Star))

tArrayOf :: Type -> Type
tArrayOf = TApp (TCon (TC "Array" (Kfun Star Star)) "prelude")

tArray :: Type
tArray = tArrayOf (TVar (TV "a" Star))

tByteArray :: Type
tByteArray = TCon (TC "ByteArray" Star) "prelude"

tDictionaryOf :: Type -> Type -> Type
tDictionaryOf keyType = TApp (TApp (TCon (TC "Dictionary" (Kfun Star (Kfun Star Star))) "prelude") keyType)

tTuple2 :: Type
tTuple2 = TCon (TC "(,)" (Kfun Star (Kfun Star Star))) "prelude"

tTuple2Of :: Type -> Type -> Type
tTuple2Of tKey tValue = TApp (TApp tTuple2 tKey) tValue

tTuple3 :: Type
tTuple3 = TCon (TC "(,,)" (Kfun Star (Kfun Star (Kfun Star Star)))) "prelude"

tTuple4 :: Type
tTuple4 = TCon (TC "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))) "prelude"

tTuple5 :: Type
tTuple5 = TCon (TC "(,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))) "prelude"

tTuple6 :: Type
tTuple6 = TCon (TC "(,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))) "prelude"

tTuple7 :: Type
tTuple7 = TCon
  (TC "(,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))))
  "prelude"

tTuple8 :: Type
tTuple8 = TCon
  (TC "(,,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
  "prelude"

tTuple9 :: Type
tTuple9 = TCon
  (TC "(,,,,,,,,)"
      (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
  )
  "prelude"

tTuple10 :: Type
tTuple10 = TCon
  (TC
    "(,,,,,,,,,)"
    (Kfun
      Star
      (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
    )
  )
  "prelude"

tTuple11 :: Type
tTuple11 = TCon
  (TC
    "(,,,,,,,,,,)"
    (Kfun
      Star
      (Kfun
        Star
        (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
      )
    )
  )
  "prelude"

tTuple12 :: Type
tTuple12 = TCon
  (TC
    "(,,,,,,,,,,,)"
    (Kfun
      Star
      (Kfun
        Star
        (Kfun
          Star
          (Kfun Star
                (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))))
          )
        )
      )
    )
  )
  "prelude"

tArrow :: Type
tArrow = TCon (TC "(->)" (Kfun Star (Kfun Star Star))) "prelude"

getTupleCtor :: Int -> Type
getTupleCtor n = case n of
  2  -> tTuple2
  3  -> tTuple3
  4  -> tTuple4
  5  -> tTuple5
  6  -> tTuple6
  7  -> tTuple7
  8  -> tTuple8
  9  -> tTuple9
  10 -> tTuple10
  11 -> tTuple11
  12 -> tTuple12
  _ -> undefined

infixr      4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TApp (TApp tArrow a) b


predClass :: Pred -> Id
predClass (IsIn i _ _) = i


predTypes :: Pred -> [Type]
predTypes (IsIn _ ts _) = ts


type Id = String

data Kind  = Star | Kfun Kind Kind
             deriving (Eq, Show, Ord)

data Pred   = IsIn Id [Type] (Maybe Area)
              deriving (Show, Ord)

instance Eq Pred where
  (==) (IsIn id ts _) (IsIn id' ts' _) = id == id' && ts == ts'

data Qual t = [Pred] :=> t
              deriving (Eq, Show, Ord)

data Scheme = Forall [Kind] (Qual Type)
              deriving (Eq, Show, Ord)


type Substitution = M.Map TVar Type

nullSubst :: Substitution
nullSubst = M.empty


preds :: Qual a -> [Pred]
preds qual = case qual of
  preds :=> _ ->
    preds


getQualified :: Qual a -> a
getQualified (_ :=> a) = a


class HasKind t where
  kind :: t -> Kind
instance HasKind TVar where
  kind (TV _ k) = k
instance HasKind TCon where
  kind (TC _ k) = k
instance HasKind Type where
  kind (TCon tc _) = kind tc
  kind (TVar u   ) = kind u
  kind (TApp t _ ) = case kind t of
    (Kfun _ k) -> k
    k          -> k
  kind _ = Star

buildKind :: Int -> Kind
buildKind n | n > 0     = Kfun Star $ buildKind (n - 1)
            | otherwise = Star

kindLength :: Kind -> Int
kindLength k = case k of
  Star -> 1
  Kfun k1 k2 -> kindLength k1 + kindLength k2


unqualify :: Qual a -> a
unqualify (_ :=> a) = a


searchVarInType :: Id -> Type -> Maybe Type
searchVarInType id t = case t of
  TVar (TV n _) ->
    if n == id then Just t else Nothing

  TCon _ _ ->
    Nothing

  TApp l r ->
    let l' = searchVarInType id l
        r' = searchVarInType id r
    in  case (l', r') of
          (Just x, _     ) -> Just x
          (_     , Just x) -> Just x
          _                -> Nothing

  _ ->
    Nothing


isTVar :: Type -> Bool
isTVar t = case t of
  TVar _ ->
    True

  _ ->
    False


isRecordType :: Type -> Bool
isRecordType t = case t of
  TRecord _ _ ->
    True

  _ ->
    False

getTRecordFieldNames :: Type -> [String]
getTRecordFieldNames t = case t of
  TRecord fields _ ->
    M.keys fields

  _ ->
    []


baseToList :: Maybe Type -> [Type]
baseToList maybeBase = case maybeBase of
  Just t  -> [t]
  Nothing -> []

collectVars :: Type -> [TVar]
collectVars t = case t of
  TVar tv         -> [tv]
  TApp    l  r    -> collectVars l `union` collectVars r
  TRecord fs base -> nub $ concat $ collectVars <$> M.elems fs <> baseToList base
  _               -> []


collectPredVars :: Pred -> [TVar]
collectPredVars (IsIn _ ts _) = nub $ concat $ collectVars <$> ts


getConstructorCon :: Type -> Type
getConstructorCon t = case t of
  TCon    _ _ -> t
  TApp    l r -> getConstructorCon l
  TRecord _ _ -> t
  _           -> t

mergeRecords :: Type -> Type -> Type
mergeRecords t1 t2 = case (t1, t2) of
  (TRecord fields1 base1, TRecord fields2 base2) -> TRecord (M.unionWith mergeRecords fields1 fields2) base1

  (TApp l r, TApp l' r') -> TApp (mergeRecords l l') (mergeRecords r r')

  _ -> t2

isFunctionType :: Type -> Bool
isFunctionType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) _) _ -> True
  _ -> False


isListType :: Type -> Bool
isListType t = case t of
  TApp (TCon (TC "List" _) "prelude") _ ->
    True

  _ ->
    False

isDictionaryType :: Type -> Bool
isDictionaryType t = case t of
  TApp (TApp (TCon (TC "Dictionary" _) "prelude") _) _ ->
    True

  _ ->
    False

isPlaceholderDict :: Qual Type -> Bool
isPlaceholderDict qt =
  qt == [] :=> TVar (TV "dict" Star)

isArrayType :: Type -> Bool
isArrayType t = case t of
  TApp (TCon (TC "Array" _) "prelude") _ ->
    True

  _ ->
    False

isByteArrayType :: Type -> Bool
isByteArrayType t = case t of
  TCon (TC "ByteArray" _) "prelude" ->
    True

  _ ->
    False

isTCon :: Type -> Bool
isTCon t = case t of
  TCon _ _ ->
    True

  TApp l r ->
    isTCon l

  _ ->
    False

getTConName :: Type -> String
getTConName t = case t of
  TCon (TC n _) _ ->
    n

  TApp l r ->
    getTConName l

  _ ->
    ""

getTConPath :: Type -> String
getTConPath t = case t of
  TCon (TC _ _) path ->
    path

  TApp l r ->
    getTConPath l

  _ ->
    ""

getReturnType :: Type -> Type
getReturnType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) _) r ->
    getReturnType r

  or ->
    or

getParamType :: Type -> Type
getParamType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) p) _ -> p


getParamTypes :: Type -> [Type]
getParamTypes t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) p) n -> p : getParamTypes n
  _ -> []


dropFirstParamType :: Type -> Type
dropFirstParamType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) p) n ->
    n

  t' ->
    t'


dropNFirstParamTypes :: Int -> Type -> Type
dropNFirstParamTypes n t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) from) to ->
    if n > 0 then
      dropNFirstParamTypes (n - 1) to
    else
      to

  t' ->
    t'


getTypeVarsInType :: Type -> [Type]
getTypeVarsInType t = case t of
  TVar _           -> [t]
  TApp l r         -> getTypeVarsInType l ++ getTypeVarsInType r
  TRecord fields _ -> concat $ getTypeVarsInType <$> M.elems fields
  _                -> []


getParamTypeOrSame :: Type -> Type
getParamTypeOrSame t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) p) _ -> p
  _ -> t


hasNumberPred :: [Pred] -> Bool
hasNumberPred ps = case ps of
  (p : next) -> case p of
    IsIn "Number" _ _ ->
      True

    _ ->
      hasNumberPred next

  [] ->
    False

-- TODO: still incomplete
selectPredsForType :: [Pred] -> Type -> [Pred]
selectPredsForType ps t = case ps of
  (p@(IsIn _ ts _) : more) ->
    if head ts == t then
      p : selectPredsForType more t
    else
      selectPredsForType more t

  [] ->
    []
