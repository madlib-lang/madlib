module Explain.Meta where

import Explain.Location


data Info a
  = NthArg Int
  | Origin a 
  deriving(Eq, Show)

-- TODO: Consider that approach
-- data Info a
--   = Info 
--       { infoNthArg :: Maybe Int
--       , infoOrigin :: Maybe a
--       }

data Meta a
  = Meta [Info a] Area a
  | Located Area a
  deriving(Eq, Show)


getArea :: Meta a -> Area
getArea (Located a _) = a
getArea (Meta _ a _)  = a


getOrigin :: Meta a -> Maybe (Info a)
getOrigin (Located _ _)        = Nothing
getOrigin (Meta [] _ _)        = Nothing
getOrigin (Meta (i:is) area a) = case i of
  Origin e -> Just $ Origin e

  _        -> getOrigin (Meta is area a) 


getNthArg :: Meta a -> Maybe (Info a)
getNthArg (Located _ _)        = Nothing
getNthArg (Meta [] _ _)        = Nothing
getNthArg (Meta (i:is) area a) = case i of
  NthArg e -> Just $ NthArg e

  _        -> getNthArg (Meta is area a) 

getNthArgFromInfos :: [Info a] -> Maybe Int
getNthArgFromInfos []     = Nothing
getNthArgFromInfos (i:is) = case i of
  NthArg e -> Just e

  _        -> getNthArgFromInfos is 
