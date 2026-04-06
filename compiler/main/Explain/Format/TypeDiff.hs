{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Explain.Format.TypeDiff where

import           Infer.Type
import           Data.List                      ( intercalate
                                                , foldl'
                                                )
import qualified Data.Map                      as M
import qualified Prettyprinter                 as Pretty
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified Prettyprinter.Internal.Type   as Pretty
import qualified Data.Text                     as Text
import qualified Data.List                     as List
import           Infer.Exp (dedupePreds)
import           Utils.Tuple


letters :: [Char]
letters = ['a' .. 'z']


renderIndexedLetter :: Int -> String
renderIndexedLetter n =
  let alphabetSize = length letters
      base = [letters !! (n `mod` alphabetSize)]
      suffix = n `div` alphabetSize
  in  if suffix == 0 then base else base <> show suffix

renderTVar :: Int -> String
renderTVar s = letters !! (s `mod` 26) : show s


indentationSize :: Int
indentationSize = 2

data Color = Green | Yellow | Red | Grey | WhiteOnRed | WhiteOnYellow

colorWhen :: Bool -> Color -> String -> String
colorWhen when c s | when      = color c s
                   | otherwise = s

color :: Color -> String -> String
color c s = case c of
  Green ->
    "\x1b[92m" <> s <> "\x1b[0m"

  Yellow ->
    "\x1b[93m" <> s <> "\x1b[0m"

  Red ->
    "\x1b[91m" <> s <> "\x1b[0m"

  Grey ->
    "\x1b[90m" <> s <> "\x1b[0m"

  WhiteOnRed ->
    "\x1b[41m" <> s <> "\x1b[0m"

  WhiteOnYellow ->
    "\x1b[43m" <> s <> "\x1b[0m"


renderTypesWithDiff :: Bool -> Type -> Type -> (String, String)
renderTypesWithDiff color t1 t2 =
  let (_, _, docT1, docT2) = typesToDocWithDiff (mempty, mempty) (mempty, mempty) t1 t2
      (docT1', docT2')  =
        if color then
          (docT1, docT2)
        else
          (Pretty.unAnnotate docT1, Pretty.unAnnotate docT2)
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s1 = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT1')
      s2 = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT2')
  in  ( if color then
          "\x1b[0m" <> Text.unpack s1
        else
          Text.unpack s1
      , if color then
          "\x1b[0m" <> Text.unpack s2
        else
          Text.unpack s2
      )

renderSchemesWithDiff :: Bool -> Scheme -> Scheme -> (String, String)
renderSchemesWithDiff color sc1 sc2 =
  let (_, _, docT1, docT2) = schemesToDocWithDiff (mempty, mempty) (mempty, mempty) sc1 sc2
      (docT1', docT2')  =
        if color then
          (docT1, docT2)
        else
          (Pretty.unAnnotate docT1, Pretty.unAnnotate docT2)
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s1            = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT1')
      s2            = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT2')
  in  ( if color then
          "\x1b[0m" <> Text.unpack s1
        else
          Text.unpack s1
      , if color then
          "\x1b[0m" <> Text.unpack s2
        else
          Text.unpack s2
      )


renderType :: Type -> String
renderType t =
  let (_, _, docT)  = typeToDoc (mempty, mempty) t
      docT'         = Pretty.unAnnotate docT
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s             = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT')
  in  Text.unpack s


renderScheme :: Scheme -> String
renderScheme sc =
  let (_, _, docT)  = schemeToDoc (mempty, mempty) sc
      docT'         = Pretty.unAnnotate docT
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s             = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT')
  in  Text.unpack s


hkLetters :: [Char]
hkLetters = ['m' ..]


kindToStr :: Kind -> String
kindToStr k = case k of
  Star     ->
    "*"

  Kfun l r ->
    kindToStr l <> " -> " <> kindToStr r


schemeToStr :: Scheme -> String
schemeToStr (Forall _ ([] :=> t)) = prettyPrintType True t
schemeToStr (Forall _ (ps :=> t)) =
  let (vars, hkVars, predStr) = predsToStr True (mempty, mempty) (dedupePreds ps)
      (_, _, typeStr)         = prettyPrintType' True (vars, hkVars) t
  in
    if length ps > 1 then
      "(" <> predStr <> ")" <> " => " <> typeStr
    else
      predStr <> " => " <> typeStr


predsToStr :: Bool -> (M.Map Int Int, M.Map Int Int) -> [Pred] -> (M.Map Int Int, M.Map Int Int, String)
predsToStr _ (vars, hkVars) [] = (vars, hkVars, "")
predsToStr rewrite (vars, hkVars) [p] = predToStr rewrite (vars, hkVars) p
predsToStr rewrite (vars, hkVars) (p:ps)  =
  let (vars', hkVars', predStr) = predToStr rewrite (vars, hkVars) p
  in
    if null ps then
     (vars', hkVars', predStr)
    else
      let (vars'', hkVars'', predStr'') = predsToStr rewrite (vars', hkVars') ps
      in  (vars'', hkVars'', predStr <> ", " <> predStr'')


predToStr :: Bool -> (M.Map Int Int, M.Map Int Int) -> Pred -> (M.Map Int Int, M.Map Int Int, String)
predToStr rewrite (vars, hkVars) p@(IsIn cls _ _) =
  let (vars', hkVars', predStr) = predToStr' rewrite (vars, hkVars) p
  in  (vars', hkVars', cls <> " " <> predStr)

predToStr' :: Bool -> (M.Map Int Int, M.Map Int Int) -> Pred -> (M.Map Int Int, M.Map Int Int, String)
predToStr' _ (vars, hkVars) (IsIn _ [] _) = (vars, hkVars, "")
predToStr' rewrite (vars, hkVars) (IsIn cls (t:ts) _) =
  let (vars', hkVars', typeStr) = typeToParenWrappedStr rewrite (vars, hkVars) t
  in
    if null ts then
      (vars', hkVars', typeStr)
    else
      let (vars'', hkVars'', typeStr'') = predToStr' rewrite (vars', hkVars') (IsIn cls ts Nothing)
      in  (vars'', hkVars'', typeStr <> " " <> typeStr'')

typeToParenWrappedStr :: Bool -> (M.Map Int Int, M.Map Int Int) -> Type -> (M.Map Int Int, M.Map Int Int, String)
typeToParenWrappedStr rewrite (vars, hkVars) t =
  let (vars', hkVars', typeStr) = prettyPrintType' rewrite (vars, hkVars) t
  in  case t of
    TApp _ _ -> (vars', hkVars', "(" <> typeStr <> ")")
    _        -> (vars', hkVars', typeStr)


prettyPrintQualType :: Qual Type -> String
prettyPrintQualType qt =
  schemeToStr (Forall [] qt)


pushAnnotation :: ann -> Pretty.Doc ann -> Pretty.Doc ann
pushAnnotation ann doc = case doc of
  Pretty.Fail     -> Pretty.Annotated ann Pretty.Fail
  Pretty.Empty    -> Pretty.Annotated ann Pretty.Empty
  Pretty.Char c   -> Pretty.Annotated ann $ Pretty.Char c
  Pretty.Text l t -> Pretty.Annotated ann $ Pretty.Text l t
  Pretty.Line     -> Pretty.Annotated ann Pretty.Line

  Pretty.FlatAlt x y     -> Pretty.Annotated ann $ Pretty.FlatAlt (pushAnnotation ann x) (pushAnnotation ann y)
  Pretty.Cat x y         -> Pretty.Annotated ann $ Pretty.Cat (pushAnnotation ann x) (pushAnnotation ann y)
  Pretty.Nest i x        -> Pretty.Annotated ann $ Pretty.Nest i (pushAnnotation ann x)
  Pretty.Union x y       -> Pretty.Annotated ann $ Pretty.Union (pushAnnotation ann x) (pushAnnotation ann y)
  Pretty.Column f        -> Pretty.Annotated ann $ Pretty.Column (pushAnnotation ann . f)
  Pretty.WithPageWidth f -> Pretty.Annotated ann $ Pretty.WithPageWidth (pushAnnotation ann . f)
  Pretty.Nesting f       -> Pretty.Annotated ann $ Pretty.Nesting (pushAnnotation ann . f)
  Pretty.Annotated _ x   -> Pretty.Annotated ann (pushAnnotation ann x)


gatherAllFnArgsForDiff :: Type -> Type -> [(Type, Type)]
gatherAllFnArgsForDiff t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _ _) tl1) tr1, TApp (TApp (TCon (TC "(->)" _) _ _) tl2) tr2) ->
    (tl1, tl2) : gatherAllFnArgsForDiff tr1 tr2

  _ ->
    [(t1, t2)]


constructorAndFunctionArgsToDocsWithDiff :: Bool -> (M.Map Int Int, M.Map Int Int) -> (M.Map Int Int, M.Map Int Int) -> [(Type, Type)] -> ((M.Map Int Int, M.Map Int Int), (M.Map Int Int, M.Map Int Int), [Pretty.Doc Terminal.AnsiStyle], [Pretty.Doc Terminal.AnsiStyle])
constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1 vars2 ts = case ts of
  ((t1@(TApp _ _), t2@(TApp _ _)) : next) | not (isTuple t1 || isTuple t2) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  ( vars1''
        , vars2''
        , if isFunctionArg && not (isFunctionType t1) then
            t1' : next1
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t1') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next1
        , if isFunctionArg&& not (isFunctionType t2) then
            t2' : next2
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t2') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next2
        )

  ((t1@(TApp _ _), t2) : next) | not (isTuple t1) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  ( vars1''
        , vars2''
        , if isFunctionArg && not (isFunctionType t1) then
            t1' : next1
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t1') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next1
        , t2' : next2
        )

  ((t1, t2@(TApp _ _)) : next) | not (isTuple t2) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  ( vars1''
        , vars2''
        , t1' : next1
        , if isFunctionArg && not (isFunctionType t2) then
            t2' : next2
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t2') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next2
        )

  ((t1, t2) : next) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  (vars1'', vars2'', t1' : next1, t2' : next2)

  [] ->
    (vars1, vars2, [], [])


gatherAllConstructorArgsForDiff :: Type -> Type -> [(Type, Type)]
gatherAllConstructorArgsForDiff t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _ _) _) _, TApp (TApp (TCon (TC "(->)" _) _ _) _) _) ->
    [(t1, t2)]

  (TApp (TApp (TCon (TC "(->)" _) _ _) _) _, _) ->
    [(t1, t2)]

  (_, TApp (TApp (TCon (TC "(->)" _) _ _) _) _) ->
    [(t1, t2)]

  (TApp l1 r1, TApp l2 r2) ->
    gatherAllConstructorArgsForDiff l1 l2 ++ [(r1, r2)]

  _ ->
    [(t1, t2)]


predsToDocsWithDiff :: (M.Map Int Int, M.Map Int Int)
  -> (M.Map Int Int, M.Map Int Int)
  -> [Pred]
  -> [Pred]
  -> ((M.Map Int Int, M.Map Int Int), (M.Map Int Int, M.Map Int Int), [Pretty.Doc Terminal.AnsiStyle], [Pretty.Doc Terminal.AnsiStyle])
predsToDocsWithDiff (vars1, hkVars1) (vars2, hkVars2) ps1 ps2 = case (ps1, ps2) of
  (IsIn cls1 ts1 _ : more1, IsIn cls2 ts2 _ : more2) ->
    let (vars1', hkVars1', ts1')             = constructorAndFunctionArgsToDocs False (vars1, hkVars1) ts1
        (vars2', hkVars2', ts2')             = constructorAndFunctionArgsToDocs False (vars2, hkVars2) ts2
        (allVars1, allVars2, more1', more2') = predsToDocsWithDiff (vars1', hkVars1') (vars2', hkVars2') more1 more2
        areEqual = cls1 == cls2 && ts1 == ts2
    in  ( allVars1
        , allVars2
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Red) (Pretty.pretty cls1)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Red)) <$> ts1')
              )
              <> Pretty.line'
            )
          ) : more1'
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Green) (Pretty.pretty cls1)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Green)) <$> ts2')
              )
              <> Pretty.line'
            )
          ) : more2'
        )

  (IsIn cls1 ts1 _ : more1, []) ->
    let (vars1', hkVars1', ts1')             = constructorAndFunctionArgsToDocs False (vars1, hkVars1) ts1
        (allVars1, allVars2, more1', _) = predsToDocsWithDiff (vars1', hkVars1') (vars2, hkVars2) more1 []
    in  ( allVars1
        , allVars2
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (Terminal.color Terminal.Red) (Pretty.pretty cls1)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (Terminal.color Terminal.Red)) <$> ts1')
              )
              <> Pretty.line'
            )
          ) : more1'
        , []
        )

  ([], IsIn cls2 ts2 _ : more2) ->
    let (vars2', hkVars2', ts2')        = constructorAndFunctionArgsToDocs False (vars1, hkVars1) ts2
        (allVars1, allVars2, _, more2') = predsToDocsWithDiff (vars1, hkVars1) (vars2', hkVars2') more2 []
    in  ( allVars1
        , allVars2
        , []
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (Terminal.color Terminal.Green) (Pretty.pretty cls2)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (Terminal.color Terminal.Green)) <$> ts2')
              )
              <> Pretty.line'
            )
          ) : more2'
        )

  ([], []) ->
    ((vars1, hkVars1), (vars2, hkVars2), [], [])


schemesToDocWithDiff :: (M.Map Int Int, M.Map Int Int)
  -> (M.Map Int Int, M.Map Int Int)
  -> Scheme
  -> Scheme
  -> ((M.Map Int Int, M.Map Int Int), (M.Map Int Int, M.Map Int Int), Pretty.Doc Terminal.AnsiStyle, Pretty.Doc Terminal.AnsiStyle)
schemesToDocWithDiff (vars1, hkVars1) (vars2, hkVars2) sc1 sc2 = case (sc1, sc2) of
  (Forall _ ([] :=> t1), Forall _ ([] :=> t2)) ->
    typesToDocWithDiff (vars1, hkVars1) (vars2, hkVars2) t1 t2

  (Forall _ (ps1 :=> t1), Forall _ (ps2 :=> t2)) ->
    let (vars1', vars2', ps1', ps2')  = predsToDocsWithDiff (vars1, hkVars1) (vars2, hkVars2) (dedupePreds ps1) (dedupePreds ps2)
        (vars1'', vars2'', t1', t2') = typesToDocWithDiff vars1' vars2' t1 t2
    in
        ( vars1''
        , vars2''
        , if length ps1 > 1 then
            Pretty.group (
              Pretty.annotate (Terminal.color Terminal.Black) Pretty.lparen
              <> Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps1')
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t1'
                )
            )
          else
            Pretty.group (
              Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps1')
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t1'
                )
            )
        , if length ps2 > 1 then
            Pretty.group (
              Pretty.annotate (Terminal.color Terminal.Black) Pretty.lparen
              <> Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps2')
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t2'
                )
            )
          else
            Pretty.group (
              Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps2')
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t2'
                )
            )
        )


typesToDocWithDiff :: (M.Map Int Int, M.Map Int Int)
  -> (M.Map Int Int, M.Map Int Int)
  -> Type
  -> Type
  -> ((M.Map Int Int, M.Map Int Int), (M.Map Int Int, M.Map Int Int), Pretty.Doc Terminal.AnsiStyle, Pretty.Doc Terminal.AnsiStyle)
typesToDocWithDiff vars1 vars2 t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _ _) _) _, TApp (TApp (TCon (TC "(->)" _) _ _) _) _) ->
    let allArgs = gatherAllFnArgsForDiff t1 t2
        (vars1', vars2', ts1, ts2) = constructorAndFunctionArgsToDocsWithDiff True vars1 vars2 allArgs
    in  ( vars1'
        , vars2'
        , Pretty.group $ Pretty.hcat $ List.intersperse (Pretty.line <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "-> ")) (Pretty.annotate (Terminal.color Terminal.Black) <$> ts1)
        , Pretty.group $ Pretty.hcat $ List.intersperse (Pretty.line <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "-> ")) (Pretty.annotate (Terminal.color Terminal.Black) <$> ts2)
        )

  (TApp (TApp (TCon (TC "(->)" _) _ _) _) _, _) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in  ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)

  (_, TApp (TApp (TCon (TC "(->)" _) _ _) _) _) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in  ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)

  (TApp (TApp (TCon (TC "(,)" _) _ _) tl1) tr1, TApp (TApp (TCon (TC "(,)" _) _ _) tl2) tr2) ->
    let (vars1', vars2', tl1', tl2')   = typesToDocWithDiff vars1 vars2 tl1 tl2
        (vars1'', vars2'', tr1', tr2') = typesToDocWithDiff vars1' vars2' tr1 tr2
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''
        , vars2''
        , Pretty.group (
            Pretty.nest indentationSize (openTuple <> Pretty.annotate (Terminal.color Terminal.Black) tl1' <> separator <> Pretty.annotate (Terminal.color Terminal.Black) tr1')
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (openTuple <> Pretty.annotate (Terminal.color Terminal.Black) tl2' <> separator <> Pretty.annotate (Terminal.color Terminal.Black) tr2')
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TCon (TC "(,,)" _) _ _) t11) t12) t13, TApp (TApp (TApp (TCon (TC "(,,)" _) _ _) t21) t22) t23) ->
    let (vars1', vars2', t11', t21')     = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')   = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23') = typesToDocWithDiff vars1'' vars2'' t13 t23
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''
        , vars2'''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _ _) t11) t12) t13) t14, TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _ _) t21) t22) t23) t24) ->
    let (vars1', vars2', t11', t21')       = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')     = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')   = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24') = typesToDocWithDiff vars1''' vars2''' t14 t24
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''
        , vars2''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _ _) t11) t12) t13) t14) t15, TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _ _) t21) t22) t23) t24) t25) ->
    let (vars1', vars2', t11', t21')         = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')       = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')     = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')   = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25') = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''''
        , vars2'''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _ _) t11) t12) t13) t14) t15) t16, TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _ _) t21) t22) t23) t24) t25) t26) ->
    let (vars1', vars2', t11', t21')           = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')         = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')       = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')     = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')   = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26') = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''''
        , vars2''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) t11) t12) t13) t14) t15) t16) t17, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) t21) t22) t23) t24) t25) t26) t27) ->
    let (vars1', vars2', t11', t21')             = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')           = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')         = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')       = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')     = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')   = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27') = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''''''
        , vars2'''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _ _) t11) t12) t13) t14) t15) t16) t17) t18, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _ _) t21) t22) t23) t24) t25) t26) t27) t28) ->
    let (vars1', vars2', t11', t21')               = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')             = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')           = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')         = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')       = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')     = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')   = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28') = typesToDocWithDiff vars1''''''' vars2''''''' t18 t28
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''''''
        , vars2''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t18'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t28'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _ _) t11) t12) t13) t14) t15) t16) t17) t18) t19, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _ _) t21) t22) t23) t24) t25) t26) t27) t28) t29) ->
    let (vars1', vars2', t11', t21')                 = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')               = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')             = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')           = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')         = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')       = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')     = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28')   = typesToDocWithDiff vars1''''''' vars2''''''' t18 t28
        (vars1''''''''', vars2''''''''', t19', t29') = typesToDocWithDiff vars1'''''''' vars2'''''''' t19 t29
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''''''''
        , vars2'''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t18' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t19'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t28' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t29'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,,)" _) _ _) t11) t12) t13) t14) t15) t16) t17) t18) t19) t110, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _ _) t21) t22) t23) t24) t25) t26) t27) t28) t29) t210) ->
    let (vars1', vars2', t11', t21')                     = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')                   = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')                 = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')               = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')             = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')           = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')         = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28')       = typesToDocWithDiff vars1''''''' vars2''''''' t18 t28
        (vars1''''''''', vars2''''''''', t19', t29')     = typesToDocWithDiff vars1'''''''' vars2'''''''' t19 t29
        (vars1'''''''''', vars2'''''''''', t110', t210') = typesToDocWithDiff vars1''''''''' vars2''''''''' t110 t210
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''''''''
        , vars2''''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t18' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t19' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t110'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t28' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t29' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t210'
            )
            <> closeTuple
          )
        )

  (t1@(TApp _ _), t2@(TApp _ _)) | length (gatherAllConstructorArgs t1) /= length (gatherAllConstructorArgs t2) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in  ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)

  (TApp _ _, TApp _ _) ->
    let allArgs = gatherAllConstructorArgsForDiff t1 t2
        (vars1', vars2', ctor1 : args1, ctor2 : args2) = constructorAndFunctionArgsToDocsWithDiff False vars1 vars2 allArgs
    in  ( vars1'
        , vars2'
        , Pretty.group (
            Pretty.annotate (Terminal.color Terminal.Black) ctor1
            <> Pretty.nest indentationSize
                (
                  Pretty.line <> Pretty.hcat (List.intersperse Pretty.line (Pretty.annotate (Terminal.color Terminal.Black) <$> args1))
                )
          )
        , Pretty.group (
            Pretty.annotate (Terminal.color Terminal.Black) ctor2
            <> Pretty.nest indentationSize
                (
                  Pretty.line <> Pretty.hcat (List.intersperse Pretty.line (Pretty.annotate (Terminal.color Terminal.Black) <$> args2))
                )
          )
        )

  (TRecord fields1 base1 _, TRecord fields2 base2 _) ->
    let allFields1 = case base1 of
          Just (TRecord fields _ _) ->
            fields <> fields1
          _ ->
            fields1
        allFields2 = case base2 of
          Just (TRecord fields _ _) ->
            fields <> fields2
          _ ->
            fields2
        ((finalVars1, finalHkVars1), (finalVars2, finalHkVars2), compiledFields1, compiledFields2) =
            foldl'
                (\(allVars1, allVars2, compiledFields1', compiledFields2') (fieldName, fieldType1) ->
                    case M.lookup fieldName allFields2 of
                      Just fieldType2 ->
                        let (allVars1', allVars2', pretty1, pretty2) = typesToDocWithDiff allVars1 allVars2 fieldType1 fieldType2
                        in  (allVars1', allVars2', compiledFields1' ++ [Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty1], compiledFields2' ++ [Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty2])

                      Nothing ->
                        let (vars1', hkVars1', pretty1) = typeToDoc allVars1 fieldType1
                        in  ((vars1', hkVars1'), allVars2, compiledFields1' ++ [Pretty.annotate (Terminal.color Terminal.Red) $ Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty1], compiledFields2')
                )
                (vars1, vars2, [], [])
              $ M.toList allFields1

        missingFields = allFields2 M.\\ allFields1
        ((finalVars2', finalHkVars2'), compiledMissingFields') =
            foldl'
                (\(allVars2, compiledFields2') (fieldName, fieldType2) ->
                    let (vars2', hkVars2', pretty2) = typeToDoc allVars2 fieldType2
                    in  ((vars2', hkVars2'), compiledFields2' ++ [pushAnnotation (Terminal.color Terminal.Green) $ Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty2])
                )
                ((finalVars2, finalHkVars2), [])
              $ M.toList missingFields
        (finalVars1', formattedBase1)   = case base1 of
          Just t1  ->
            let (vars, hkVars, pretty) = typeToDoc (finalVars1, finalHkVars1) t1
            in ((vars, hkVars), pushAnnotation (Terminal.color Terminal.Red) $ Pretty.pretty "..." <> pretty <> Pretty.comma <> Pretty.line)

          Nothing ->
            ((finalVars1, finalHkVars1), Pretty.emptyDoc)

        (finalVars2'', formattedBase2)   = case base2 of
          Just t2  ->
            let (vars, hkVars, pretty) = typeToDoc (finalVars2', finalHkVars2') t2
            in ((vars, hkVars), pushAnnotation (Terminal.color Terminal.Green) $ Pretty.pretty "..." <> pretty <> Pretty.comma <> Pretty.line)

          Nothing ->
            ((finalVars2', finalHkVars2'), Pretty.emptyDoc)

        compiled1 = Pretty.annotate (Terminal.color Terminal.Black) $
            Pretty.group (
              Pretty.lbrace <> Pretty.nest indentationSize (
                Pretty.line
                <> formattedBase1
                <> Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.line) (Pretty.annotate (Terminal.color Terminal.Black) <$> compiledFields1))
              )
              <> (if null compiledFields1 then Pretty.emptyDoc else Pretty.line)
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rbrace
            )
        compiled2 = Pretty.annotate (Terminal.color Terminal.Black) $
            Pretty.group (
              Pretty.lbrace <> Pretty.nest indentationSize (
                Pretty.line
                <> formattedBase2
                <> Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.line) (Pretty.annotate (Terminal.color Terminal.Black) <$> (compiledFields2 ++ compiledMissingFields')))
              )
              <> (if null compiledFields2 then Pretty.emptyDoc else Pretty.line)
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rbrace
            )
    in  (finalVars1', finalVars2'', compiled1, compiled2)

  (t1, t2) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in if t1 /= t2 then
      ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)
    else
      ((vars1', hkVars1'), (vars2', hkVars2'), pretty1, pretty2)



prettyPrintType :: Bool -> Type -> String
prettyPrintType rewrite =
  lst . prettyPrintType' rewrite (mempty, mempty)


prettyPrintType' :: Bool -> (M.Map Int Int, M.Map Int Int) -> Type -> (M.Map Int Int, M.Map Int Int, String)
prettyPrintType' rewrite (vars, hkVars) t = case t of
  TCon (TC n _) _ _ ->
    (vars, hkVars, n)

  TVar (TV n k)   ->
    if not rewrite then
      (vars, hkVars, renderTVar n)
    else
      case k of
        Star -> case M.lookup n vars of
          Just x ->
            (vars, hkVars, renderIndexedLetter x)

          Nothing ->
            let newIndex = M.size vars
            in  (M.insert n newIndex vars, hkVars, renderIndexedLetter newIndex)

        Kfun _ _ -> case M.lookup n hkVars of
          Just x ->
            (vars, hkVars, [hkLetters !! x])

          Nothing ->
            let newIndex = M.size hkVars
            in  (vars, M.insert n newIndex hkVars, [hkLetters !! newIndex])

  TApp (TApp (TCon (TC "(,)" _) _ _) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, "#[" <> left <> ", " <> right <> "]")

  TApp (TApp (TApp (TCon (TC "(,,)" _) _ _) tl) tr) trr ->
    let (varsLeft      , hkVarsLeft      , left      ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = prettyPrintType' rewrite (varsRight, hkVarsRight) trr
    in  (varsRightRight, hkVarsRightRight, "#[" <> left <> ", " <> right <> ", " <> rightRight <> "]")

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _ _) tl) tr) trr) trrr ->
    let (varsLeft      , hkVarsLeft      , left      ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = prettyPrintType' rewrite (varsRight, hkVarsRight) trr
        (_, _, rightRightRight) =
            prettyPrintType' rewrite (varsRightRight, hkVarsRightRight) trrr
    in  ( varsRightRight
        , hkVarsRightRight
        , "#[" <> left <> ", " <> right <> ", " <> rightRight <> ", " <> rightRightRight <> "]"
        )

  TApp (TApp (TCon (TC "(->)" _) _ _) tl) tr ->
    let (varsLeft, hkVarsLeft, left) = case tl of
          TApp (TApp (TCon (TC "(->)" _) _ _) tl') tr' ->
            let (varsLeft' , hkVarsLeft' , left' ) = prettyPrintType' rewrite (vars, hkVars) tl'
                (varsRight', hkVarsRight', right') = prettyPrintType' rewrite (varsLeft', hkVarsLeft') tr'
                leftParenthesis                    = case tl' of
                  TApp (TApp (TCon (TC "(->)" _) _ _) _) _ ->
                    True

                  _ ->
                    False
                left'' = if leftParenthesis then "(" <> left' <> ")" else left'
            in  (varsRight', hkVarsRight', "(" <> left'' <> " -> " <> right' <> ")")

          _ -> prettyPrintType' rewrite (vars, hkVars) tl

        (varsRight, hkVarsRight, right) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, left <> " -> " <> right)

  TApp tl tr ->
    let (varsLeft , hkVarsLeft , left ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = case tr of
          TApp _ _ ->
            let (varsRight', hkVarsRight', right') = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
            in  if not (isTuple tr)
                  then (varsRight', hkVarsRight', "(" <> right' <> ")")
                  else (varsRight', hkVarsRight', right')
          _ -> prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, left <> " " <> right)

  -- TODO: Add spreads display
  TRecord fields base _ ->
    let (finalVars, finalHkVars, compiledFields) =
            foldl'
                (\(vars', hkVars', compiledFields') (fieldName, fieldType) ->
                  let (vars'', hkVars'', compiledField) = prettyPrintType' rewrite (vars', hkVars') fieldType
                  in  (vars'', hkVars'', compiledFields' ++ [(fieldName, compiledField)])
                )
                (vars, hkVars, [])
              $ M.toList fields
        compiledFields' = (\(fieldName, fieldType) -> fieldName <> " :: " <> fieldType) <$> compiledFields
        formattedBase   = case base of
          Just _  -> "...base, "
          Nothing -> ""
        compiled = "{ " <> formattedBase <> intercalate ", " compiledFields' <> " }"
    in  (finalVars, finalHkVars, compiled)

  TGen n ->
    if not rewrite then
      (vars, hkVars, "T" <> show n)
    else
      case M.lookup (n - 1000) vars of
        Just x  ->
          (vars, hkVars, renderIndexedLetter x)

        Nothing ->
          let newIndex = M.size vars
          in  (M.insert (n - 1000) newIndex vars, hkVars, renderIndexedLetter newIndex)

  _ ->
    (vars, hkVars, "")


gatherAllFnArgs :: Type -> [Type]
gatherAllFnArgs t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) tl) tr ->
    tl : gatherAllFnArgs tr

  _ ->
    [t]


gatherAllConstructorArgs :: Type -> [Type]
gatherAllConstructorArgs t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) _) _ ->
    [t]

  TApp l r ->
    gatherAllConstructorArgs l ++ [r]

  _ ->
    [t]


constructorAndFunctionArgsToDocs :: Bool -> (M.Map Int Int, M.Map Int Int) -> [Type] -> (M.Map Int Int, M.Map Int Int, [Pretty.Doc ann])
constructorAndFunctionArgsToDocs isFunctionArg (vars, hkVars) ts = case ts of
  (t@(TApp _ _) : next) | not (isTuple t) ->
    let (vars', hkVvars', t')     = typeToDoc (vars, hkVars) t
        (vars'', hkVars'', next') = constructorAndFunctionArgsToDocs isFunctionArg (vars', hkVvars') next
    in  ( vars'', hkVars''
        , if isFunctionArg && not (isFunctionType t) then
            t' : next'
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t') <> Pretty.line' <> Pretty.rparen) : next'
        )

  (t : next) ->
    let (vars', hkVars', t')      = typeToDoc (vars, hkVars) t
        (vars'', hkVars'', next') = constructorAndFunctionArgsToDocs isFunctionArg (vars', hkVars') next
    in  (vars'', hkVars'', t' : next')

  [] ->
    (vars, hkVars, [])



predsToDocs :: (M.Map Int Int, M.Map Int Int) -> [Pred] -> (M.Map Int Int, M.Map Int Int, [Pretty.Doc ann])
predsToDocs (vars, hkVars) ps = case ps of
  (IsIn cls ts _ : more) ->
    let (vars', hkVars', ts')     = constructorAndFunctionArgsToDocs False (vars, hkVars) ts
        (vars'', hkVars'', more') = predsToDocs (vars', hkVars') more
    in  ( vars''
        , hkVars''
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.pretty cls
                <> Pretty.hcat ((Pretty.line <>) <$> ts')
              )
              <> Pretty.line'
            )
          ) : more'
        )

  [] ->
    (vars, hkVars, [])


schemeToDoc :: (M.Map Int Int, M.Map Int Int) -> Scheme -> (M.Map Int Int, M.Map Int Int, Pretty.Doc ann)
schemeToDoc (vars, hkVars) sc = case sc of
  Forall _ ([] :=> t) ->
    typeToDoc (vars, hkVars) t

  Forall _ (ps :=> t) ->
    let (vars', hkVars', ps')  = predsToDocs (vars, hkVars) (dedupePreds ps)
        (vars'', hkVars'', t') = typeToDoc (vars', hkVars') t
    in
      if length ps > 1 then
        ( vars''
        , hkVars''
        , Pretty.lparen <> Pretty.hcat ps' <> Pretty.rparen <> Pretty.pretty " => " <> t'
        )
      else
        ( vars''
        , hkVars''
        , Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.space) ps') <> Pretty.pretty " => " <> t'
        )


typeToDoc :: (M.Map Int Int, M.Map Int Int) -> Type -> (M.Map Int Int, M.Map Int Int, Pretty.Doc ann)
typeToDoc (vars, hkVars) t = case t of
  TCon (TC n _) _ _ ->
    (vars, hkVars, Pretty.pretty n)

  TVar (TV n k)   ->
    case k of
      Star -> case M.lookup n vars of
        Just x ->
          (vars, hkVars, Pretty.pretty (renderIndexedLetter x))

        Nothing ->
          let newIndex = M.size vars
          in  (M.insert n newIndex vars, hkVars, Pretty.pretty (renderIndexedLetter newIndex))

      Kfun _ _ -> case M.lookup n hkVars of
        Just x ->
          (vars, hkVars, Pretty.pretty [hkLetters !! x])

        Nothing ->
          let newIndex = M.size hkVars
          in  (vars, M.insert n newIndex hkVars, Pretty.pretty [hkLetters !! newIndex])

  TApp (TApp (TCon (TC "(,)" _) _ _) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = typeToDoc (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = typeToDoc (varsLeft, hkVarsLeft) tr
    in  ( varsRight
        , hkVarsRight
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#[" <> Pretty.line' <> left <> Pretty.comma <> Pretty.line <> right
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TCon (TC "(,,)" _) _ _) t1) t2) t3 ->
    let (vars' , hkVars' , t1')   = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')   = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3') = typeToDoc (vars'', hkVars'') t3
    in  ( vars'''
        , hkVars'''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#[" <> Pretty.line' <> t1' <> Pretty.comma <> Pretty.line <> t2' <> Pretty.comma <> Pretty.line <> t3'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _ _) t1) t2) t3) t4 ->
    let (vars' , hkVars' , t1')   = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')   = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3') = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4') = typeToDoc (vars''', hkVars''') t4
    in  ( vars''''
        , hkVars''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#[" <> Pretty.line' <> t1' <> Pretty.comma <> Pretty.line <> t2' <> Pretty.comma <> Pretty.line <> t3' <> Pretty.comma <> Pretty.line <> t4'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _ _) t1) t2) t3) t4) t5 ->
    let (vars' , hkVars' , t1')   = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')   = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3') = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4') = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5') = typeToDoc (vars'''', hkVars'''') t5
    in  ( vars'''''
        , hkVars'''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _ _) t1) t2) t3) t4) t5) t6 ->
    let (vars' , hkVars' , t1')         = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')         = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')       = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')     = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')   = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6') = typeToDoc (vars''''', hkVars''''') t6
    in  ( vars''''''
        , hkVars''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) t1) t2) t3) t4) t5) t6) t7 ->
    let (vars' , hkVars' , t1')           = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')           = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')         = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')       = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')     = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')   = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7') = typeToDoc (vars'''''', hkVars'''''') t7
    in  ( vars'''''''
        , hkVars'''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) t1) t2) t3) t4) t5) t6) t7) t8 ->
    let (vars' , hkVars' , t1')             = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')             = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')           = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')         = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')       = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')     = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7')   = typeToDoc (vars'''''', hkVars'''''') t7
        (vars'''''''', hkVars'''''''', t8') = typeToDoc (vars''''''', hkVars''''''') t8
    in  ( vars''''''''
        , hkVars''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7' <> Pretty.comma
              <> Pretty.line <> t8'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) t1) t2) t3) t4) t5) t6) t7) t8) t9 ->
    let (vars' , hkVars' , t1')               = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')               = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')             = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')           = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')         = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')       = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7')     = typeToDoc (vars'''''', hkVars'''''') t7
        (vars'''''''', hkVars'''''''', t8')   = typeToDoc (vars''''''', hkVars''''''') t8
        (vars''''''''', hkVars''''''''', t9') = typeToDoc (vars'''''''', hkVars'''''''') t9
    in  ( vars'''''''''
        , hkVars'''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7' <> Pretty.comma
              <> Pretty.line <> t8' <> Pretty.comma
              <> Pretty.line <> t9'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) t1) t2) t3) t4) t5) t6) t7) t8) t9) t10 ->
    let (vars' , hkVars' , t1')                  = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')                  = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')                = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')              = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')            = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')          = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7')        = typeToDoc (vars'''''', hkVars'''''') t7
        (vars'''''''', hkVars'''''''', t8')      = typeToDoc (vars''''''', hkVars''''''') t8
        (vars''''''''', hkVars''''''''', t9')    = typeToDoc (vars'''''''', hkVars'''''''') t9
        (vars'''''''''', hkVars'''''''''', t10') = typeToDoc (vars''''''''', hkVars''''''''') t10
    in  ( vars''''''''''
        , hkVars''''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7' <> Pretty.comma
              <> Pretty.line <> t8' <> Pretty.comma
              <> Pretty.line <> t9' <> Pretty.comma
              <> Pretty.line <> t10'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  (TApp (TApp (TCon (TC "(->)" _) _ _) _) _) ->
    let allArgs = gatherAllFnArgs t
        (vars', hkVars', args) = constructorAndFunctionArgsToDocs True (vars, hkVars) allArgs
    in  ( vars'
        , hkVars'
        , Pretty.hcat $ List.intersperse (Pretty.softline <> Pretty.pretty "-> ") args
        )

  (TApp _ _) ->
    let allArgs = gatherAllConstructorArgs t
        (vars', hkVars', ctor : args) = constructorAndFunctionArgsToDocs False (vars, hkVars) allArgs
    in  ( vars'
        , hkVars'
        , Pretty.group (
            ctor
            <> Pretty.nest indentationSize
                (
                  Pretty.line <> Pretty.hcat (List.intersperse Pretty.line args)
                )
          )
        )

  TRecord fields base _ ->
    let (finalVars, finalHkVars, compiledFields) =
            foldl'
                (\(vars', hkVars', compiledFields') (fieldName, fieldType) ->
                  let (vars'', hkVars'', compiledField) = typeToDoc (vars', hkVars') fieldType
                  in  (vars'', hkVars'', compiledFields' ++ [(fieldName, compiledField)])
                )
                (vars, hkVars, [])
              $ M.toList fields
        compiledFields' = (\(fieldName, fieldType) -> Pretty.pretty fieldName <> Pretty.pretty " :: " <> fieldType) <$> compiledFields
        formattedBase   = case base of
          Just _  -> Pretty.pretty "...base," <> Pretty.line
          Nothing -> Pretty.emptyDoc
        compiled = Pretty.group (
            Pretty.lbrace <> Pretty.nest indentationSize (
              Pretty.line
              <> formattedBase
              <> Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.line) compiledFields')
            )
            <> Pretty.line
            <> Pretty.rbrace
          )
    in  (finalVars, finalHkVars, compiled)

  TGen n ->
    case M.lookup (n - 1000) vars of
      Just x  ->
        (vars, hkVars, Pretty.pretty (renderIndexedLetter x))

      Nothing ->
        let newIndex = M.size vars
        in  (M.insert (n - 1000) newIndex vars, hkVars, Pretty.pretty (renderIndexedLetter newIndex))

  _ ->
    (vars, hkVars, Pretty.emptyDoc)


isTuple :: Type -> Bool
isTuple t = case t of
  TApp (TApp (TCon (TC "(,)" _) _ _) _) _ ->
    True

  TApp (TApp (TApp (TCon (TC "(,,)" _) _ _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _ _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _ _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _ _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _ _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _ _) _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _ _) _) _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,,)" _) _ _) _) _) _) _) _) _) _) _) _) _ ->
    True

  _ -> False
