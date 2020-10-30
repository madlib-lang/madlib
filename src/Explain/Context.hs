module Explain.Context where

import Infer.Type

data Loc = Loc Int Int Int deriving(Eq, Show)

data Meta a = Meta Loc (Maybe Type) a


setType :: Meta a -> Type -> Meta a
setType (Meta l _ a) t = Meta l (Just t) a


-- data Context a = Context Loc Type a