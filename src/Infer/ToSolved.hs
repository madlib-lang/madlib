module Infer.ToSolved where

import qualified AST.Canonical as Can
import qualified AST.Solved as Slv
import Infer.Type
import qualified Data.Map as M


tSubst :: Type
tSubst = TVar (TV "-" Star)

toSolved :: Can.Exp -> Slv.Exp
toSolved (Can.Canonical area exp) = case exp of
  Can.LNum v -> Slv.Solved tNumber area (Slv.LNum v)
  Can.LStr v -> Slv.Solved tStr area (Slv.LStr v)
  Can.LBool v -> Slv.Solved tBool area (Slv.LBool v)
  Can.LUnit -> Slv.Solved tUnit area Slv.LUnit

  Can.Var v -> Slv.Solved tSubst area (Slv.Var v)

  Can.App f arg closed ->
    Slv.Solved tSubst area (Slv.App (toSolved f) (toSolved arg) closed)

  Can.Abs (Can.Canonical area param) body ->
    Slv.Solved tSubst area (Slv.Abs (Slv.Solved tSubst area param) (toSolved <$> body))

  Can.TemplateString exps ->
    Slv.Solved tSubst area (Slv.TemplateString (toSolved <$> exps))

  Can.Access rec field ->
    Slv.Solved tSubst area (Slv.Access (toSolved rec) (toSolved field))

  Can.Assignment name exp ->
    Slv.Solved tSubst area (Slv.Assignment name (toSolved exp))

  Can.TypedExp exp sc ->
    Slv.Solved tSubst area (Slv.TypedExp (toSolved exp) sc)

  Can.Record fields ->
    Slv.Solved tSubst area (Slv.Record (fieldToSolved <$> fields))

  Can.If cond truthy falsy ->
    Slv.Solved tSubst area (Slv.If (toSolved cond) (toSolved truthy) (toSolved falsy))

  Can.Where exp iss ->
    Slv.Solved tSubst area (Slv.Where (toSolved exp) (isToSolved <$> iss))

  Can.Export exp -> Slv.Solved tSubst area (Slv.Export (toSolved exp))

  Can.ListConstructor items -> Slv.Solved tSubst area (Slv.ListConstructor (liToSolved <$> items))

  Can.TupleConstructor exps ->
    Slv.Solved tSubst area (Slv.TupleConstructor (toSolved <$> exps))

  Can.JSExp code -> Slv.Solved tSubst area (Slv.JSExp code)

  Can.JSXExpChild exp -> toSolved exp


liToSolved :: Can.ListItem -> Slv.ListItem
liToSolved (Can.Canonical area li) = case li of
  Can.ListItem exp -> Slv.Solved tSubst area (Slv.ListItem $ toSolved exp)

  Can.ListSpread exp -> Slv.Solved tSubst area (Slv.ListSpread $ toSolved exp)


fieldToSolved :: Can.Field -> Slv.Field
fieldToSolved (Can.Canonical area field) = case field of
  Can.Field (name, exp) -> Slv.Solved tSubst area $ Slv.Field (name, toSolved exp)

  Can.FieldSpread exp -> Slv.Solved tSubst area $ Slv.FieldSpread (toSolved exp)

isToSolved :: Can.Is -> Slv.Is
isToSolved (Can.Canonical area (Can.Is pat exp)) =
  Slv.Solved tSubst area (Slv.Is (patternToSolved pat) (toSolved exp))

patternToSolved :: Can.Pattern -> Slv.Pattern
patternToSolved (Can.Canonical area pat) = case pat of
  Can.PVar name -> Slv.Solved tSubst area (Slv.PVar name)

  Can.PAny -> Slv.Solved tSubst area Slv.PAny

  Can.PCtor name pats -> Slv.Solved tSubst area (Slv.PCtor name (patternToSolved <$> pats))

  Can.PNum v -> Slv.Solved tSubst area (Slv.PNum v)

  Can.PStr v -> Slv.Solved tSubst area (Slv.PStr v)

  Can.PBool v -> Slv.Solved tSubst area (Slv.PBool v)

  Can.PCon name -> Slv.Solved tSubst area (Slv.PCon name)

  Can.PRecord fields -> Slv.Solved tSubst area (Slv.PRecord (M.map patternToSolved fields))

  Can.PList items -> Slv.Solved tSubst area (Slv.PList (patternToSolved <$> items))

  Can.PTuple items -> Slv.Solved tSubst area (Slv.PTuple (patternToSolved <$> items))

  Can.PSpread pat -> Slv.Solved tSubst area (Slv.PSpread (patternToSolved pat))
