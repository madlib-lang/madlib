{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}
module Driver.Query where

import qualified Rock
import qualified AST.Source                 as Src
import qualified AST.Canonical              as Can
import qualified Canonicalize.Env           as Can
import qualified AST.Solved                 as Slv
-- import           Parse.Madlib.AST
import           Error.Error (CompilationError(CompilationError))
import           Data.GADT.Compare.TH (deriveGEq)
import           Data.Some
import           Data.Hashable

data Query a where
  File :: FilePath -> Query String
  ParsedAST :: FilePath -> FilePath -> Query Src.AST
  CanonicalizedAST :: FilePath -> Query (Can.AST, Can.Env)
  -- SolvedAST :: FilePath -> Query Slv.AST
  -- BuiltTarget :: FilePath -> Query Slv.AST

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt = \case
    File path ->
      hashWithSalt salt (path, 0 :: Int)

    ParsedAST rootPath path ->
      hashWithSalt salt (rootPath <> ":" <> path, 1 :: Int)

    CanonicalizedAST path ->
      hashWithSalt salt (path, 2 :: Int)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query

-- rules :: Rock.Rules Query
-- rules = \case
--   Parse path -> do
--     source <- liftIO $ readFile path
--     return $ buildAST path source


-- runQuery :: Query a -> IO a
-- runQuery query = do
--   memoVar <- newIORef mempty
--   let task = Rock.fetch query
--   Rock.runTask (Rock.memoise memoVar rules) task


-- parse :: FilePath -> IO (Either CompilationError Src.AST)
-- parse = runQuery . Parse


-- -- input :: (Monoid w, Functor f) => f a -> f (a, w)
-- -- input = fmap (, mempty)

-- {-# language FlexibleInstances #-}
-- {-# language GADTs #-}
-- {-# language StandaloneDeriving #-}
-- {-# language DeriveAnyClass #-}
-- {-# language TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wno-missing-methods #-}
-- module Driver.Driver where
-- import Control.Monad.IO.Class
-- import Data.GADT.Compare.TH (deriveGEq)
-- import Data.Hashable
-- import Data.Some
-- import Data.IORef
-- import qualified Rock
-- import Data.GADT.Compare (GEq)

-- data Query a where
--   A :: Query Integer
--   B :: Query Integer
--   C :: Query Integer
--   D :: Query Integer

-- deriving instance Show (Query a)

-- -- deriving instance GEq Query
-- deriveGEq ''Query
-- -- instance GEq Query where

-- instance Hashable (Query a) where
--   hashWithSalt salt query =
--     case query of
--       A -> hashWithSalt salt (0 :: Int)
--       B -> hashWithSalt salt (1 :: Int)
--       C -> hashWithSalt salt (2 :: Int)
--       D -> hashWithSalt salt (3 :: Int)

-- instance Hashable (Some Query) where
--   hashWithSalt salt (Some query) = hashWithSalt salt query

-- rules :: Rock.Rules Query
-- rules key = do
--   liftIO $ putStrLn $ "Fetching " <> show key
--   case key of
--     A -> pure 10
--     B -> do
--       a <- Rock.fetch A
--       pure $ a + 20
--     C -> do
--       a <- Rock.fetch A
--       pure $ a + 30
--     D ->
--       (+) <$> Rock.fetch B <*> Rock.fetch C

-- main :: IO ()
-- main = do
--   do
--     liftIO $ putStrLn "Running"
--     result <- Rock.runTask rules (Rock.fetch D)
--     print result
--   do
--     liftIO $ putStrLn "Running with memoisation"
--     memoVar <- newIORef mempty
--     result <- Rock.runTask (Rock.memoise memoVar rules) (Rock.fetch D)
--     liftIO $ print result
