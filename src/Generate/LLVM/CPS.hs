{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Generate.LLVM.CPS where

import AST.Solved
import Data.Text
import qualified Data.Text as T
import Data.Unique
import Infer.Type
import qualified Data.List as List


-- toCPS :: AST -> AST
-- toCPS ast =
--   ast { aexps = expToCPS <$> aexps ast }

-- expToCPS :: Exp -> Exp
-- expToCPS exp = case exp of
--   Solved _ _ (LNum _) ->
--     exp

--   Solved t area (Abs param body) ->
--     undefined




-- gensym :: IO String
-- gensym = fmap render newUnique where
--   render u =
--     let hu = hashUnique u
--     in  "$v" <> show hu


-- continuationResultType :: Type
-- continuationResultType = TVar (TV "kresult" Star)

-- buildType :: Type -> Type
-- buildType t =
--   let paramTypes = getParamTypes t
--       retType = getReturnType t
--   in  List.foldr fn (List.head paramTypes) (List.tail paramTypes) `fn` (retType `fn` continuationResultType) `fn` continuationResultType

-- buildContinuationType :: Type -> Type
-- buildContinuationType t =
--   let retType = getReturnType t
--   in  retType `fn` continuationResultType

-- m :: Exp -> IO Exp
-- m expr = case expr of
--   Solved (ps :=> t) area (Abs param [cexpr]) -> do
--     k       <- gensym
--     xformed <- tc cexpr (Solved (ps :=> buildContinuationType t) area (Var k))
--     return (Solved (ps :=> buildType t) area (Abs param [Solved (ps :=> buildType t) area (Abs (Solved (ps :=> buildContinuationType t) area k) [xformed])]))

--   Solved qt area (Var n)  -> return (Solved qt area (Var n))

--   Solved _ _ App {} -> error "non-atomic expression"

-- tc :: Exp -> Exp -> IO Exp
-- tc expr c = case expr of
--   Solved _ _ Abs {} -> do
--     aexpr <- m expr
--     return (CApp c [aexpr])

--   Solved _ _ (Var _) -> do
--     aexpr <- m expr
--     return (CApp c [aexpr])

--   Solved _ _ (App f e final) -> do
--     let cexpr fs = tk e (\es -> return (CApp fs [es, c]))
--     tk f cexpr

-- tk :: Exp -> (Exp -> IO Exp) -> IO Exp
-- tk expr k = case expr of
--   Solved _ _ Abs {} -> do
--     aexpr <- m expr
--     k aexpr

--   Solved _ _ Var {} -> do
--     aexpr <- m expr
--     k aexpr

--   Solved _ _ (App f e final) -> do
--     rv <- gensym
--     xformed <- k (AVar rv)

--     let cont     = ALam [rv] xformed
--         cexpr fs = tk e (\es -> return (CApp fs [es, cont]))

--     tk f cexpr
