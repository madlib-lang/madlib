{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
module Driver.Rules_ where

-- import qualified Rock
-- import qualified AST.Source                 as Src
-- import qualified AST.Canonical              as Can
-- import qualified AST.Solved                 as Slv
-- import           Error.Error (CompilationError(CompilationError))
-- import           Data.IORef
-- import           Parse.Madlib.AST
-- import           Control.Monad.IO.Class
-- import           Driver.Query

-- -- rules :: Rock.Rules Query
-- -- rules query = case query of
-- --   File path ->
-- --     liftIO $ readFile path

-- --   ParsedAST path -> do
-- --     source <- Rock.fetch $ File path
-- --     return $ buildAST path source

-- --   _ ->
-- --     undefined

-- -- runQuery :: Query a -> IO a
-- -- runQuery query = do
-- --   memoVar <- newIORef mempty
-- --   let task = Rock.fetch query
-- --   Rock.runTask (Rock.memoise memoVar rules) task


-- -- parse :: FilePath -> IO (Either CompilationError Src.AST)
-- -- parse = runQuery . ParsedAST







-- rules :: Rock.GenRules (Rock.Writer [CompilationError] Query) Query
-- rules (Rock.Writer query) = case query of
--   File path ->
--     input $ liftIO $ readFile path

--   ParsedAST path _ -> nonInput $ do
--     source <- Rock.fetch $ File path
--     case buildAST path source of
--       Right ast ->
--         return (ast, mempty)

--       Left err ->
--         return (Src.AST {}, [err])



-- input :: (Monoid w, Functor f) => f a -> f (a, w)
-- input = fmap (, mempty)

-- nonInput :: Functor f => f (a, w) -> f (a, w)
-- nonInput = id

-- runQuery :: Query a -> IO a
-- runQuery query = do
--   memoVar <- newIORef mempty
--   let task = Rock.fetch query
--   Rock.runTask (Rock.memoise memoVar (Rock.writer (\_ _ -> return ()) rules)) task
-- -- runQuery :: Query (Rock.Writer [CompilationError] Query a) -> IO (Either [CompilationError] a)
-- -- runQuery query = do
-- --   memoVar <- newIORef mempty
-- --   let task = Rock.fetch query
-- --   let r = rules mempty
-- --   let tt = Rock.writer mempty rules
-- --   Rock.runTask tt r
--   -- let tt = Rock.writer mempty rules
--   -- let memoized = Rock.memoise memoVar tt
  
--   -- run <- Rock.runTask tt task
--   -- case run of
--   --   Rock.Writer q -> do
--   --     r <- Rock.runTask tt (Rock.fetch q)
--   --     return r
--   -- return (Left [])
--   -- case run of
--   --   Rock.Writer (a, []) ->
--   --     return $ Right a


-- -- parse :: FilePath -> IO Src.AST
-- -- parse path = do
-- --   runQuery $ ParsedAST path
