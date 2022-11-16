module Infer.ToSolved where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Type
import           Infer.Typing
import qualified Data.Map                      as M


tSubst :: Type
tSubst = TVar (TV "-" Star)

toSolved :: Can.Exp -> Slv.Exp
toSolved (Can.Canonical area exp) = case exp of
  Can.LNum  v          -> Slv.Typed ([] :=> tNumber) area (Slv.LNum v)
  Can.LFloat v         -> Slv.Typed ([] :=> tFloat) area (Slv.LFloat v)
  Can.LStr  v          -> Slv.Typed ([] :=> tStr) area (Slv.LStr v)
  Can.LChar v          -> Slv.Typed ([] :=> tChar) area (Slv.LChar v)
  Can.LBool v          -> Slv.Typed ([] :=> tBool) area (Slv.LBool v)
  Can.LUnit            -> Slv.Typed ([] :=> tUnit) area Slv.LUnit
  Can.TypedHole        -> Slv.Typed ([] :=> tUnit) area Slv.TypedHole

  Can.Var v            -> Slv.Typed ([] :=> tSubst) area (Slv.Var v False)

  Can.App f arg closed -> Slv.Typed ([] :=> tSubst) area (Slv.App (toSolved f) (toSolved arg) closed)

  Can.Abs (Can.Canonical paramArea param) body ->
    Slv.Typed ([] :=> tSubst) area (Slv.Abs (Slv.Typed ([] :=> tSubst) paramArea param) (toSolved <$> body))

  Can.TemplateString exps    -> Slv.Typed ([] :=> tSubst) area (Slv.TemplateString (toSolved <$> exps))

  Can.Access     (Can.Canonical _ (Can.Var namespace))  (Can.Canonical _ (Can.Var fieldName))  ->
    Slv.Typed ([] :=> tSubst) area (Slv.Var (namespace <> fieldName) False)

  Can.Access     rec  field  -> Slv.Typed ([] :=> tSubst) area (Slv.Access (toSolved rec) (toSolved field))

  Can.Assignment name exp    -> Slv.Typed ([] :=> tSubst) area (Slv.Assignment name (toSolved exp))

  Can.TypedExp exp typing sc -> Slv.Typed ([] :=> tSubst) area (Slv.TypedExp (toSolved exp) (updateTyping typing) sc)

  Can.Record fields          -> Slv.Typed ([] :=> tSubst) area (Slv.Record (fieldToSolved <$> fields))

  Can.If cond truthy falsy   -> Slv.Typed ([] :=> tSubst) area (Slv.If (toSolved cond) (toSolved truthy) (toSolved falsy))

  Can.Where exp iss          -> Slv.Typed ([] :=> tSubst) area (Slv.Where (toSolved exp) (isToSolved <$> iss))

  Can.Export           exp   -> Slv.Typed ([] :=> tSubst) area (Slv.Export (toSolved exp))

  Can.NameExport       name  -> Slv.Typed ([] :=> tSubst) area (Slv.NameExport name)

  Can.TypeExport       name  -> Slv.Typed ([] :=> tSubst) area (Slv.TypeExport name)

  Can.ListConstructor  items -> Slv.Typed ([] :=> tSubst) area (Slv.ListConstructor (liToSolved <$> items))

  Can.TupleConstructor exps  -> Slv.Typed ([] :=> tSubst) area (Slv.TupleConstructor (toSolved <$> exps))

  Can.JSExp            code  -> Slv.Typed ([] :=> tSubst) area (Slv.JSExp code)

  Can.Do               exps  -> Slv.Typed ([] :=> tSubst) area (Slv.Do (toSolved <$> exps))

  Can.Extern (Forall _ qt) localName foreignName ->
    Slv.Typed ([] :=> tSubst) area (Slv.Extern qt localName foreignName)



liToSolved :: Can.ListItem -> Slv.ListItem
liToSolved (Can.Canonical area li) = case li of
  Can.ListItem   exp -> Slv.Typed ([] :=> tSubst) area (Slv.ListItem $ toSolved exp)

  Can.ListSpread exp -> Slv.Typed ([] :=> tSubst) area (Slv.ListSpread $ toSolved exp)


fieldToSolved :: Can.Field -> Slv.Field
fieldToSolved (Can.Canonical area field) = case field of
  Can.Field       (name, exp) -> Slv.Typed ([] :=> tSubst) area $ Slv.Field (name, toSolved exp)

  Can.FieldSpread exp         -> Slv.Typed ([] :=> tSubst) area $ Slv.FieldSpread (toSolved exp)

isToSolved :: Can.Is -> Slv.Is
isToSolved (Can.Canonical area (Can.Is pat exp)) = Slv.Typed ([] :=> tSubst) area (Slv.Is (patternToSolved pat) (toSolved exp))

patternToSolved :: Can.Pattern -> Slv.Pattern
patternToSolved (Can.Canonical area pat) = case pat of
  Can.PVar name       -> Slv.Typed ([] :=> tSubst) area (Slv.PVar name)

  Can.PAny            -> Slv.Typed ([] :=> tSubst) area Slv.PAny

  Can.PCon name pats  -> Slv.Typed ([] :=> tSubst) area (Slv.PCon name (patternToSolved <$> pats))

  Can.PNum    v       -> Slv.Typed ([] :=> tSubst) area (Slv.PNum v)

  Can.PChar    v      -> Slv.Typed ([] :=> tSubst) area (Slv.PChar v)

  Can.PStr    v       -> Slv.Typed ([] :=> tSubst) area (Slv.PStr v)

  Can.PBool   v       -> Slv.Typed ([] :=> tSubst) area (Slv.PBool v)

  Can.PRecord fields  -> Slv.Typed ([] :=> tSubst) area (Slv.PRecord (M.map patternToSolved fields))

  Can.PList   items   -> Slv.Typed ([] :=> tSubst) area (Slv.PList (patternToSolved <$> items))

  Can.PTuple  items   -> Slv.Typed ([] :=> tSubst) area (Slv.PTuple (patternToSolved <$> items))

  Can.PSpread pat     -> Slv.Typed ([] :=> tSubst) area (Slv.PSpread (patternToSolved pat))
