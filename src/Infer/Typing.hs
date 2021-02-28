module Infer.Typing where

import qualified AST.Solved                    as Slv
import qualified AST.Canonical                 as Can
import Infer.Type

-- TODO: Probably the whole typing needs to become a scheme in its solved form
updateTyping :: Can.Typing -> Slv.Typing
updateTyping typing = case typing of
  Can.Canonical area (Can.TRSingle name     ) -> Slv.Untyped area $ Slv.TRSingle name

  Can.Canonical area (Can.TRComp name vars  ) -> Slv.Untyped area $ Slv.TRComp name (updateTyping <$> vars)

  Can.Canonical area (Can.TRArr  l    r     ) -> Slv.Untyped area $ Slv.TRArr (updateTyping l) (updateTyping r)

  Can.Canonical area (Can.TRRecord fields   ) -> Slv.Untyped area $ Slv.TRRecord (updateTyping <$> fields)

  Can.Canonical area (Can.TRTuple  elems    ) -> Slv.Untyped area $ Slv.TRTuple (updateTyping <$> elems)

  Can.Canonical area (Can.TRConstrained ts t) -> Slv.Untyped area $ Slv.TRConstrained (updateTyping <$> ts) (updateTyping t)
