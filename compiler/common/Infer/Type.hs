{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Infer.Type where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           AST.Source                     ( Exp )
import           Data.List                      ( nub
                                                , union
                                                )
import           Explain.Location
import           Data.Hashable
import           GHC.Generics hiding(Constructor)
import           Control.Applicative ((<|>))
import           Text.Show.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU


data TVar = TV Int Kind
  deriving (Show, Eq, Ord, Generic, Hashable)

data TCon = TC Id Kind
  deriving (Show, Eq, Ord, Generic, Hashable)

data Type
  = TVar TVar                      -- Variable type
  | TCon TCon FilePath {-# UNPACK #-} !Int -- Constructor type - FilePath of where that type is defined, Int is hash of FilePath for fast equality
  | TGen Int
  | TApp Type Type                 -- Arrow type
  | TRecord (M.Map Id Type) (Maybe Type) (M.Map Id Type)
  -- ^ Maybe Type is the extended record type, most likely a type variable
  -- and the last Map is the optional fields due to unifying extensible with non extensible records
  | TAlias FilePath Id [TVar] Type -- Aliases, filepath of definition module, name, params, type it aliases
  deriving (Generic)

-- Custom Show instance that omits the internal Int hash from TCon
-- to maintain backward compatibility with golden test files
instance Show Type where
  showsPrec p (TVar tv)              = showParen (p > 10) $
    showString "TVar " . showsPrec 11 tv
  showsPrec p (TCon tc fp _)         = showParen (p > 10) $
    showString "TCon " . showsPrec 11 tc . showChar ' ' . showsPrec 11 fp
  showsPrec p (TGen n)               = showParen (p > 10) $
    showString "TGen " . shows n
  showsPrec p (TApp l r)             = showParen (p > 10) $
    showString "TApp " . showsPrec 11 l . showChar ' ' . showsPrec 11 r
  showsPrec p (TRecord f b o)        = showParen (p > 10) $
    showString "TRecord " . showsPrec 11 f . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 o
  showsPrec p (TAlias fp n vs t)     = showParen (p > 10) $
    showString "TAlias " . showsPrec 11 fp . showChar ' ' . showsPrec 11 n . showChar ' ' . showsPrec 11 vs . showChar ' ' . showsPrec 11 t

instance Eq Type where
  TVar a       == TVar b        = a == b
  TCon tc1 _ h1 == TCon tc2 _ h2 = h1 == h2 && tc1 == tc2
  TGen a       == TGen b        = a == b
  TApp l1 r1   == TApp l2 r2    = l1 == l2 && r1 == r2
  TRecord f1 b1 o1 == TRecord f2 b2 o2 = f1 == f2 && b1 == b2 && o1 == o2
  TAlias p1 n1 vs1 t1 == TAlias p2 n2 vs2 t2 = p1 == p2 && n1 == n2 && vs1 == vs2 && t1 == t2
  _ == _ = False

instance Ord Type where
  compare (TVar a)            (TVar b)            = compare a b
  compare (TCon tc1 fp1 h1)   (TCon tc2 fp2 h2)   = compare tc1 tc2 <> compare h1 h2 <> compare fp1 fp2
  compare (TGen a)            (TGen b)             = compare a b
  compare (TApp l1 r1)        (TApp l2 r2)         = compare l1 l2 <> compare r1 r2
  compare (TRecord f1 b1 o1)  (TRecord f2 b2 o2)  = compare f1 f2 <> compare b1 b2 <> compare o1 o2
  compare (TAlias p1 n1 vs1 t1) (TAlias p2 n2 vs2 t2) = compare p1 p2 <> compare n1 n2 <> compare vs1 vs2 <> compare t1 t2
  compare x y = compare (typeTag x) (typeTag y)
    where
      typeTag :: Type -> Int
      typeTag TVar{}   = 0
      typeTag TCon{}   = 1
      typeTag TGen{}   = 2
      typeTag TApp{}   = 3
      typeTag TRecord{} = 4
      typeTag TAlias{} = 5

instance Hashable Type where
  hashWithSalt s (TVar tv)         = s `hashWithSalt` (0 :: Int) `hashWithSalt` tv
  hashWithSalt s (TCon tc _ h)     = s `hashWithSalt` (1 :: Int) `hashWithSalt` tc `hashWithSalt` h
  hashWithSalt s (TGen n)          = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (TApp l r)        = s `hashWithSalt` (3 :: Int) `hashWithSalt` l `hashWithSalt` r
  hashWithSalt s (TRecord f b o)   = s `hashWithSalt` (4 :: Int) `hashWithSalt` f `hashWithSalt` b `hashWithSalt` o
  hashWithSalt s (TAlias p n vs t) = s `hashWithSalt` (5 :: Int) `hashWithSalt` p `hashWithSalt` n `hashWithSalt` vs `hashWithSalt` t


-- | Smart constructor: builds a TCon with a precomputed FilePath hash for fast equality.
mkTCon :: TCon -> FilePath -> Type
mkTCon tc fp = TCon tc fp (hash fp)
{-# INLINE mkTCon #-}


infixr `TApp`

getTConId :: TCon -> Id
getTConId (TC id _) = id

getTVarId :: TVar -> Int
getTVarId (TV id _) = id

getTV :: Type -> TVar
getTV t = case t of
  TVar tv ->
    tv

  _ ->
    undefined


tVar :: Int -> Type
tVar v = TVar (TV v Star)


tNumber :: Type
tNumber = TVar (TV 0 Star)


qtNumber :: Qual Type
qtNumber = [IsIn "Number" [tNumber] Nothing] :=> tNumber


tShort :: Type
tShort = mkTCon (TC "Short" Star) "prelude"


tFloat :: Type
tFloat = mkTCon (TC "Float" Star) "prelude"


tInteger :: Type
tInteger = mkTCon (TC "Integer" Star) "prelude"


tByte :: Type
tByte = mkTCon (TC "Byte" Star) "prelude"


qNumber :: Qual Type
qNumber = [IsIn "Number" [TVar (TV 0 Star)] Nothing] :=> TVar (TV 0 Star)


tBool :: Type
tBool = mkTCon (TC "Boolean" Star) "prelude"


tStr :: Type
tStr = mkTCon (TC "String" Star) "prelude"


tChar :: Type
tChar = mkTCon (TC "Char" Star) "prelude"


tUnit :: Type
tUnit = mkTCon (TC "{}" Star) "prelude"


tList :: Type
tList = tListOf (TVar (TV 0 Star))


tListOf :: Type -> Type
tListOf = TApp (mkTCon (TC "List" (Kfun Star Star)) "prelude")

listItemType :: Type -> Type
listItemType t = case t of
  TApp (TCon (TC "List" (Kfun Star Star)) "prelude" _) itemType ->
    itemType



tArrayOf :: Type -> Type
tArrayOf = TApp (mkTCon (TC "Array" (Kfun Star Star)) "prelude")


tArray :: Type
tArray = tArrayOf (TVar (TV 0 Star))


tArrayCon :: Type
tArrayCon = mkTCon (TC "Array" (Kfun Star Star)) "prelude"


tByteArray :: Type
tByteArray = mkTCon (TC "ByteArray" Star) "prelude"


tTuple2Of :: Type -> Type -> Type
tTuple2Of tKey tValue = TApp (TApp tTuple2 tKey) tValue


tTuple3Of :: Type -> Type -> Type -> Type
tTuple3Of t1 t2 t3 = TApp (TApp (TApp tTuple3 t1) t2) t3


tTuple4Of :: Type -> Type -> Type -> Type -> Type
tTuple4Of t1 t2 t3 t4 = TApp (TApp (TApp (TApp tTuple4 t1) t2) t3) t4


tTuple2 :: Type
tTuple2 = mkTCon (TC "(,)" (Kfun Star (Kfun Star Star))) "prelude"


tTuple3 :: Type
tTuple3 = mkTCon (TC "(,,)" (Kfun Star (Kfun Star (Kfun Star Star)))) "prelude"


tTuple4 :: Type
tTuple4 = mkTCon (TC "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))) "prelude"


tTuple5 :: Type
tTuple5 = mkTCon (TC "(,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))) "prelude"


tTuple6 :: Type
tTuple6 = mkTCon (TC "(,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))) "prelude"


tTuple7 :: Type
tTuple7 = mkTCon
  (TC "(,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))))
  "prelude"


tTuple8 :: Type
tTuple8 = mkTCon
  (TC "(,,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
  "prelude"


tTuple9 :: Type
tTuple9 = mkTCon
  (TC "(,,,,,,,,)"
      (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
  )
  "prelude"


tTuple10 :: Type
tTuple10 = mkTCon
  (TC
    "(,,,,,,,,,)"
    (Kfun
      Star
      (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))))
    )
  )
  "prelude"


tTuple11 :: Type
tTuple11 = mkTCon
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
tTuple12 = mkTCon
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
tArrow = mkTCon (TC "(->)" (Kfun Star (Kfun Star Star))) "prelude"


getTupleCtor :: Int -> Type
getTupleCtor n = case n of
  2  ->
    tTuple2

  3  ->
    tTuple3

  4  ->
    tTuple4

  5  ->
    tTuple5

  6  ->
    tTuple6

  7  ->
    tTuple7

  8  ->
    tTuple8

  9  ->
    tTuple9

  10 ->
    tTuple10

  11 ->
    tTuple11

  12 ->
    tTuple12

  _ ->
    undefined


infixr      4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TApp (TApp tArrow a) b


predClass :: Pred -> Id
predClass (IsIn i _ _) = i


predTypes :: Pred -> [Type]
predTypes (IsIn _ ts _) = ts


predArea :: Pred -> Maybe Area
predArea (IsIn _ _ area) = area


type Id = String

data Kind
  = Star
  | Kfun Kind Kind
  deriving (Eq, Show, Ord, Generic, Hashable)

data Pred
  = IsIn Id [Type] (Maybe Area)
  deriving (Show, Ord, Generic, Hashable)

instance Eq Pred where
  (==) (IsIn id ts _) (IsIn id' ts' _) = id == id' && ts == ts'

data Qual t
  = [Pred] :=> t
  deriving (Eq, Show, Ord, Generic, Hashable)

data Scheme
  = Forall [Kind] (Qual Type)
  deriving (Eq, Show, Ord, Generic, Hashable)


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
  kind (TCon tc _ _) = kind tc
  kind (TVar u     ) = kind u
  kind (TApp t _   ) = case kind t of
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


searchVarInType :: Int -> Type -> Maybe Type
searchVarInType id t = case t of
  TVar (TV n _) ->
    if n == id then Just t else Nothing

  TCon _ _ _ ->
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
  TRecord _ _ _ ->
    True

  _ ->
    False

getTRecordFieldNames :: Type -> [String]
getTRecordFieldNames t = case t of
  TRecord fields _ _ ->
    M.keys fields

  _ ->
    []


baseToList :: Maybe Type -> [Type]
baseToList maybeBase = case maybeBase of
  Just t  -> [t]
  Nothing -> []


collectVars :: Type -> [TVar]
collectVars t = case t of
  TVar tv         ->
    [tv]

  TApp l r ->
    collectVars l `union` collectVars r

  TRecord fs base _ ->
    nub $ concat $ collectVars <$> M.elems fs <> baseToList base

  _ ->
    []


collectPredVars :: Pred -> [TVar]
collectPredVars (IsIn _ ts _) = nub $ concat $ collectVars <$> ts


getConstructorCon :: Type -> Type
getConstructorCon t = case t of
  TCon _ _ _ ->
    t

  TApp l _ ->
    getConstructorCon l

  TRecord _ _ _ ->
    t

  _ ->
    t


mergeRecords :: Type -> Type -> Type
mergeRecords t1 t2 = case (t1, t2) of
  (TRecord fields1 base1 _, TRecord fields2 _ _) ->
    TRecord (M.unionWith mergeRecords fields1 fields2) base1 mempty

  (TApp l r, TApp l' r') ->
    TApp (mergeRecords l l') (mergeRecords r r')

  _ ->
    t1


isFunctionType :: Type -> Bool
isFunctionType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) _) _ ->
    True

  _ ->
    False


isTCon :: Type -> Bool
isTCon t = case t of
  TCon _ _ _ ->
    True

  TApp l _ ->
    isTCon l

  _ ->
    False

getTConName :: Type -> String
getTConName t = case t of
  TCon (TC n _) _ _ ->
    n

  TApp l _ ->
    getTConName l

  _ ->
    ""

getTConPath :: Type -> String
getTConPath t = case t of
  TCon (TC _ _) path _ ->
    path

  TApp l _ ->
    getTConPath l

  _ ->
    ""

getAliasPath :: Type -> String
getAliasPath t = case t of
  TAlias path _ _ _ ->
    path

  _ ->
    ""

getReturnType :: Type -> Type
getReturnType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) _) r ->
    getReturnType r

  or ->
    or


getParamTypes :: Type -> [Type]
getParamTypes t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) p) n ->
    p : getParamTypes n

  _ ->
    []


dropFirstParamType :: Type -> Type
dropFirstParamType t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) _) n ->
    n

  t' ->
    t'


dropNFirstParamTypes :: Int -> Type -> Type
dropNFirstParamTypes n t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) _) to ->
    if n > 0 then
      dropNFirstParamTypes (n - 1) to
    else
      to

  t' ->
    t'


getTypeVarsInType :: Type -> [Type]
getTypeVarsInType t = case t of
  TVar _ ->
    [t]

  TApp l r ->
    getTypeVarsInType l ++ getTypeVarsInType r

  TRecord fields _ _ ->
    concat $ getTypeVarsInType <$> M.elems fields

  _ ->
    []



getParamTypeOrSame :: Type -> Type
getParamTypeOrSame t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) p) _ -> p
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
    if null ts then
      selectPredsForType more t
    else if head ts == t then
      p : selectPredsForType more t
    else
      selectPredsForType more t

  [] ->
    []


findTypeVarInType :: Int -> Type -> Maybe Type
findTypeVarInType tvName t = case t of
  TApp l r ->
    findTypeVarInType tvName l <|> findTypeVarInType tvName r

  TVar (TV n _) | n == tvName ->
    Just t

  TRecord fields base optionalFields ->
    (foldl (<|>) Nothing $ findTypeVarInType tvName <$> (M.elems fields))
    <|> (base >>= findTypeVarInType tvName)
    <|> (foldl (<|>) Nothing $ findTypeVarInType tvName <$> (M.elems optionalFields))

  _ ->
    Nothing
