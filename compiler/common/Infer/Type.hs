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


data TVar = TV {-# UNPACK #-} !Int Kind
  deriving (Show, Generic)

data TCon = TC Id Kind
  deriving (Show, Generic)

-- Manual Eq/Ord for TVar: short-circuits on Int (the unique id), only
-- comparing Kind when ids match. Generic-derived Eq/Ord builds an
-- intermediate representation and is consistently slower.
instance Eq TVar where
  TV a k1 == TV b k2 = a == b && k1 == k2
  {-# INLINE (==) #-}

instance Ord TVar where
  compare (TV a k1) (TV b k2) = case compare a b of
    EQ -> compare k1 k2
    o  -> o
  {-# INLINE compare #-}

-- TCon equality is dominated by the Id (constructor name); kinds match by
-- construction when ids match in well-typed programs.
instance Eq TCon where
  TC i1 k1 == TC i2 k2 = i1 == i2 && k1 == k2
  {-# INLINE (==) #-}

instance Ord TCon where
  compare (TC i1 k1) (TC i2 k2) = compare i1 i2 <> compare k1 k2
  {-# INLINE compare #-}

instance Hashable TVar where
  hashWithSalt s (TV i _) =
    -- The Int id is unique per TVar in a single inference run; the Kind is
    -- redundant for hashing distinctness and adds work to no benefit.
    hashWithSalt s i
  {-# INLINE hashWithSalt #-}

instance Hashable TCon where
  hashWithSalt s (TC i k) =
    s `hashWithSalt` i `hashWithSalt` k
  {-# INLINE hashWithSalt #-}

data Type
  = TVar TVar                      -- Variable type
  | TCon TCon FilePath {-# UNPACK #-} !Int -- Constructor type - FilePath of where that type is defined, Int is hash of FilePath for fast equality
  | TGen Int
  | TApp Type Type                 -- Arrow type
  | TRowEmpty
  | TRowExtend Id Type Type
  -- ^ A scoped row label.  The tail is a type of kind `Row`; equal labels in
  -- the tail are deliberately not collapsed, because the outer label shadows
  -- them (the semantics of record spread/update).
  | TRecordRow Type (M.Map Id Type)
  -- ^ A record is a row plus the short-lived JSX optional-field compatibility
  -- map.  Normal records use an empty map; this map disappears when JSX
  -- defaulting is migrated to row constraints.
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
  showsPrec _ TRowEmpty              = showString "TRowEmpty"
  showsPrec p (TRowExtend n t tail)  = showParen (p > 10) $
    showString "TRowExtend " . showsPrec 11 n . showChar ' ' . showsPrec 11 t . showChar ' ' . showsPrec 11 tail
  showsPrec p (TRecordRow row o)     = showParen (p > 10) $
    showString "TRecordRow " . showsPrec 11 row . showChar ' ' . showsPrec 11 o
  showsPrec p (TAlias fp n vs t)     = showParen (p > 10) $
    showString "TAlias " . showsPrec 11 fp . showChar ' ' . showsPrec 11 n . showChar ' ' . showsPrec 11 vs . showChar ' ' . showsPrec 11 t


visibleRow :: Type -> (M.Map Id Type, Maybe Type)
visibleRow = go
  where
    go TRowEmpty = (M.empty, Nothing)
    go (TRowExtend name fieldType tail) =
      let (fields, base) = go tail
      in  (M.insert name fieldType fields, base)
    go tail = (M.empty, Just tail)


recordRow :: Type -> Type
recordRow row = TRecordRow row M.empty


closedRecord :: M.Map Id Type -> Type
closedRecord fields = recordRow (rowFromFields fields TRowEmpty)


openRecord :: M.Map Id Type -> Type -> Type
openRecord fields tail = recordRow (rowFromFields fields tail)


recordParts :: Type -> Maybe (Type, M.Map Id Type)
recordParts (TRecordRow row optionalFields) = Just (row, optionalFields)
recordParts _ = Nothing


recordVisibleParts :: Type -> Maybe (M.Map Id Type, Maybe Type, M.Map Id Type)
recordVisibleParts (TRecordRow row optionalFields) =
  let (fields, tail) = visibleRow row
  in Just (fields, tail, optionalFields)
recordVisibleParts _ = Nothing


recordVisibleFields :: Type -> Maybe (M.Map Id Type)
recordVisibleFields record = do
  (fields, _, optionalFields) <- recordVisibleParts record
  return (fields <> optionalFields)


isClosedRecord :: Type -> Bool
isClosedRecord (TRecordRow row _) = case snd (visibleRow row) of
  Nothing -> True
  Just _  -> False
isClosedRecord _ = False


removeRowLabels :: S.Set Id -> Type -> Type
removeRowLabels labels = go
  where
    go TRowEmpty = TRowEmpty
    go (TRowExtend name fieldType tail)
      | name `S.member` labels = go tail
      | otherwise = TRowExtend name fieldType (go tail)
    go tail = tail


removeRecordLabels :: S.Set Id -> Type -> Type
removeRecordLabels labels (TRecordRow row optionalFields) =
  TRecordRow (removeRowLabels labels row) (M.withoutKeys optionalFields labels)
removeRecordLabels _ t = t


rowEmpty :: Type
rowEmpty = TRowEmpty


rowExtend :: Id -> Type -> Type -> Type
rowExtend = TRowExtend


rowFromFields :: M.Map Id Type -> Type -> Type
rowFromFields fields tail =
  foldr (\(name, fieldType) rest -> TRowExtend name fieldType rest) tail (M.toAscList fields)

instance Eq Type where
  TVar a       == TVar b        = a == b
  -- The cached path hash is only a fast rejection check.  Treating it as the
  -- identity of a module is unsound: a collision made two constructors from
  -- different modules equal while Ord still distinguished them.
  TCon tc1 fp1 h1 == TCon tc2 fp2 h2 = h1 == h2 && fp1 == fp2 && tc1 == tc2
  TGen a       == TGen b        = a == b
  TApp l1 r1   == TApp l2 r2    = l1 == l2 && r1 == r2
  TRowEmpty    == TRowEmpty     = True
  TRowExtend n1 t1 r1 == TRowExtend n2 t2 r2 = n1 == n2 && t1 == t2 && r1 == r2
  TRecordRow r1 o1 == TRecordRow r2 o2 = r1 == r2 && o1 == o2
  TAlias p1 n1 vs1 t1 == TAlias p2 n2 vs2 t2 = p1 == p2 && n1 == n2 && vs1 == vs2 && t1 == t2
  _ == _ = False

instance Ord Type where
  compare (TVar a)            (TVar b)            = compare a b
  compare (TCon tc1 fp1 h1)   (TCon tc2 fp2 h2)   = compare tc1 tc2 <> compare h1 h2 <> compare fp1 fp2
  compare (TGen a)            (TGen b)             = compare a b
  compare (TApp l1 r1)        (TApp l2 r2)         = compare l1 l2 <> compare r1 r2
  compare TRowEmpty TRowEmpty = EQ
  compare (TRowExtend n1 t1 r1) (TRowExtend n2 t2 r2) = compare n1 n2 <> compare t1 t2 <> compare r1 r2
  compare (TRecordRow r1 o1)  (TRecordRow r2 o2)  = compare r1 r2 <> compare o1 o2
  compare (TAlias p1 n1 vs1 t1) (TAlias p2 n2 vs2 t2) = compare p1 p2 <> compare n1 n2 <> compare vs1 vs2 <> compare t1 t2
  compare x y = compare (typeTag x) (typeTag y)
    where
      typeTag :: Type -> Int
      typeTag TVar{}   = 0
      typeTag TCon{}   = 1
      typeTag TGen{}   = 2
      typeTag TApp{}   = 3
      typeTag TRowEmpty = 4
      typeTag TRowExtend{} = 5
      typeTag TRecordRow{} = 6
      typeTag TAlias{} = 7

instance Hashable Type where
  hashWithSalt s (TVar tv)         = s `hashWithSalt` (0 :: Int) `hashWithSalt` tv
  hashWithSalt s (TCon tc _ h)     = s `hashWithSalt` (1 :: Int) `hashWithSalt` tc `hashWithSalt` h
  hashWithSalt s (TGen n)          = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (TApp l r)        = s `hashWithSalt` (3 :: Int) `hashWithSalt` l `hashWithSalt` r
  hashWithSalt s TRowEmpty         = s `hashWithSalt` (4 :: Int)
  hashWithSalt s (TRowExtend n t r) = s `hashWithSalt` (5 :: Int) `hashWithSalt` n `hashWithSalt` t `hashWithSalt` r
  hashWithSalt s (TRecordRow r o)  = s `hashWithSalt` (6 :: Int) `hashWithSalt` r `hashWithSalt` o
  hashWithSalt s (TAlias p n vs t) = s `hashWithSalt` (7 :: Int) `hashWithSalt` p `hashWithSalt` n `hashWithSalt` vs `hashWithSalt` t


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

runtimeTypeAt :: FilePath -> Type
runtimeTypeAt builtinsPath = mkTCon (TC "Type" Star) builtinsPath


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

isListType :: Type -> Bool
isListType t = case t of
  TApp (TCon (TC "List" _) _ _) _ -> True
  _ -> False



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
  | Row
  | Kfun Kind Kind
  deriving (Eq, Show, Ord, Generic)

instance Hashable Kind where
  hashWithSalt s Star         = s `hashWithSalt` (0 :: Int)
  hashWithSalt s Row          = s `hashWithSalt` (1 :: Int)
  hashWithSalt s (Kfun k1 k2) = s `hashWithSalt` (2 :: Int) `hashWithSalt` k1 `hashWithSalt` k2
  {-# INLINE hashWithSalt #-}

data Pred
  = IsIn Id [Type] (Maybe Area)
  deriving (Show, Generic)

-- | The semantic identity of a predicate.  Source spans are diagnostics, not
-- evidence: including them in a set/map/hash key makes otherwise-identical
-- constraints fail to deduplicate.
data PredKey = PredKey Id [Type]
  deriving (Eq, Ord, Show, Generic, Hashable)

predKey :: Pred -> PredKey
predKey (IsIn i ts _) = PredKey i ts

instance Eq Pred where
  (==) (IsIn id ts _) (IsIn id' ts' _) = id == id' && ts == ts'

instance Ord Pred where
  compare p p' = compare (predKey p) (predKey p')

instance Hashable Pred where
  hashWithSalt s = hashWithSalt s . predKey
  {-# INLINE hashWithSalt #-}

data Qual t
  = [Pred] :=> t
  deriving (Eq, Show, Ord, Generic)

instance Hashable t => Hashable (Qual t) where
  hashWithSalt s (ps :=> t) =
    s `hashWithSalt` ps `hashWithSalt` t
  {-# INLINE hashWithSalt #-}

data Scheme
  = Forall [Kind] (Qual Type)
  deriving (Eq, Show, Ord, Generic)

instance Hashable Scheme where
  hashWithSalt s (Forall ks qt) =
    s `hashWithSalt` ks `hashWithSalt` qt
  {-# INLINE hashWithSalt #-}


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
  kind TRowEmpty = Row
  kind (TRowExtend _ _ _) = Row
  kind _ = Star

buildKind :: Int -> Kind
buildKind n | n > 0     = Kfun Star $ buildKind (n - 1)
            | otherwise = Star

kindLength :: Kind -> Int
kindLength k = case k of
  Star -> 1
  Row -> 1
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

  TRowEmpty ->
    Nothing

  TRowExtend _ fieldType tail ->
    searchVarInType id fieldType <|> searchVarInType id tail

  TRecordRow row optionalFields ->
    searchVarInType id row
    <|> foldl (<|>) Nothing (searchVarInType id <$> M.elems optionalFields)

  TAlias _ _ _ aliased ->
    searchVarInType id aliased

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
  TRecordRow _ _ ->
    True

  _ ->
    False

getTRecordFieldNames :: Type -> [String]
getTRecordFieldNames t = case t of
  TRecordRow row _ ->
    M.keys (fst $ visibleRow row)

  _ ->
    []


collectVars :: Type -> [TVar]
collectVars t = case t of
  TVar tv         ->
    [tv]

  TApp l r ->
    collectVars l `union` collectVars r

  TRowEmpty ->
    []

  TRowExtend _ fieldType tail ->
    collectVars fieldType `union` collectVars tail

  TRecordRow row optionalFields ->
    nub $ collectVars row ++ concatMap collectVars (M.elems optionalFields)

  TAlias _ _ _ aliased ->
    collectVars aliased

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

  TRecordRow _ _ ->
    t

  _ ->
    t


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

  TRowEmpty ->
    []

  TRowExtend _ fieldType tail ->
    getTypeVarsInType fieldType ++ getTypeVarsInType tail

  TRecordRow row optionalFields ->
    getTypeVarsInType row
    ++ concatMap getTypeVarsInType (M.elems optionalFields)

  TAlias _ _ _ aliased ->
    getTypeVarsInType aliased

  _ ->
    []




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

  TRowEmpty ->
    Nothing

  TRowExtend _ fieldType tail ->
    findTypeVarInType tvName fieldType <|> findTypeVarInType tvName tail

  TRecordRow row optionalFields ->
    findTypeVarInType tvName row
    <|> (foldl (<|>) Nothing $ findTypeVarInType tvName <$> (M.elems optionalFields))

  TAlias _ _ _ aliased ->
    findTypeVarInType tvName aliased

  _ ->
    Nothing
