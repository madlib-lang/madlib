module Infer.Typing where

import qualified AST.Solved                    as Slv
import qualified AST.Canonical                 as Can

updateTyping :: Can.Typing -> Slv.Typing
updateTyping typing = case typing of
  Can.Canonical _ (Can.TRSingle name) -> Slv.TRSingle name

  Can.Canonical _ (Can.TRComp name vars) ->
    Slv.TRComp name (updateTyping <$> vars)

  Can.Canonical _ (Can.TRArr l r) ->
    Slv.TRArr (updateTyping l) (updateTyping r)

  Can.Canonical _ (Can.TRRecord fields) ->
    Slv.TRRecord (updateTyping <$> fields)

  Can.Canonical _ (Can.TRTuple elems) -> Slv.TRTuple (updateTyping <$> elems)

  Can.Canonical _ (Can.TRConstrained ts t) ->
    Slv.TRConstrained (updateTyping <$> ts) (updateTyping t)
