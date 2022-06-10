{-# LANGUAGE LambdaCase #-}
module Parse.Madlib.Dictionary where
import           AST.Source
import qualified Data.Map               as Map
import           Explain.Location
import Utils.PathUtils
import Utils.Path



addDictionaryImportIfNeeded :: PathUtils -> FilePath -> AST -> IO AST
addDictionaryImportIfNeeded pathUtils ctxPath ast =
  if usesDictionaryConstructor ast then
    addDictionaryImport pathUtils ctxPath ast
  else
    return ast

addDictionaryImport :: PathUtils -> FilePath -> AST -> IO AST
addDictionaryImport pathUtils ctxPath ast = do
  dictionaryModulePath <- resolveAbsoluteSrcPath pathUtils ctxPath "Dictionary"
  updatedImports       <- updateImports pathUtils ctxPath dictionaryModulePath False (aimports ast)
  return ast { aimports = updatedImports }

updateImports :: PathUtils -> FilePath -> Maybe FilePath -> Bool -> [Import] -> IO [Import]
updateImports pathUtils ctxPath dictionaryModulePath found imports = case imports of
  [] ->
    if found then
      return []
    else
      return [Source emptyArea TargetAll (DefaultImport (Source emptyArea TargetAll "__Dictionary_Auto__") "Dictionary" "")]

  (imp@(Source area target (NamedImport names path _)) : next) -> do
    absPath <- resolveAbsoluteSrcPath pathUtils ctxPath path
    if absPath == dictionaryModulePath then
      if "fromList" `elem` (getSourceContent <$> names) then
        return (imp : next)
      else
        return $ Source area target (NamedImport (Source area target "fromList" : names) "Dictionary" path) : next
    else do
      next' <- updateImports pathUtils ctxPath dictionaryModulePath found next
      return (imp : next')

  (imp@(Source _ _ (DefaultImport _ path _)) : next) -> do
    absPath <- resolveAbsoluteSrcPath pathUtils ctxPath path
    if absPath == dictionaryModulePath then
      return (imp : next)
    else do
      next' <- updateImports pathUtils ctxPath dictionaryModulePath found next
      return (imp : next')

  (imp : next) -> do
    next' <- updateImports pathUtils ctxPath dictionaryModulePath found next
    return (imp : next')

usesDictionaryConstructor :: AST -> Bool
usesDictionaryConstructor ast =
  let methods = concat $ (\(Source _ _ (Instance _ _ _ methods)) -> Map.elems methods) <$> ainstances ast
  in  any expUsesDictionaryConstructor (methods ++ aexps ast)

expUsesDictionaryConstructor :: Exp -> Bool
expUsesDictionaryConstructor exp = case exp of
  Source _ _ (Dictionary _) ->
    True

  Source _ _ (TemplateString exps) ->
    any expUsesDictionaryConstructor exps

  Source _ _ (UnOp operator operand) ->
    expUsesDictionaryConstructor operator || expUsesDictionaryConstructor operand

  Source _ _ (BinOp left operator right) ->
    expUsesDictionaryConstructor left || expUsesDictionaryConstructor operator || expUsesDictionaryConstructor right

  Source _ _ (App fn args) ->
    expUsesDictionaryConstructor fn || any expUsesDictionaryConstructor args

  Source _ _ (Abs _ body) ->
    any expUsesDictionaryConstructor body

  Source _ _ (AbsWithMultilineBody _ body) ->
    any expUsesDictionaryConstructor body

  Source _ _ (Return exp) ->
    expUsesDictionaryConstructor exp

  Source _ _ (Access name field) ->
    expUsesDictionaryConstructor name || expUsesDictionaryConstructor field

  Source _ _ (Record fields) ->
    any
      (\case
        Source _ _ (Field (_, exp)) ->
          expUsesDictionaryConstructor exp

        Source _ _ (FieldSpread exp) ->
          expUsesDictionaryConstructor exp

        _ ->
          False
      )
      fields

  Source _ _ (If cond truthy falsy) ->
    expUsesDictionaryConstructor cond || expUsesDictionaryConstructor truthy || expUsesDictionaryConstructor falsy

  Source _ _ (Ternary cond truthy falsy) ->
    expUsesDictionaryConstructor cond || expUsesDictionaryConstructor truthy || expUsesDictionaryConstructor falsy

  Source _ _ (Where exp iss) ->
    expUsesDictionaryConstructor exp
    || any
        (\case Source _ _ (Is _ exp) -> expUsesDictionaryConstructor exp)
        iss

  Source _ _ (WhereAbs iss) ->
    any
      (\case Source _ _ (Is _ exp) -> expUsesDictionaryConstructor exp)
      iss

  Source _ _ (Do exps) ->
    any expUsesDictionaryConstructor exps

  Source _ _ (DoAssignment _ exp) ->
    expUsesDictionaryConstructor exp

  Source _ _ (Export exp) ->
    expUsesDictionaryConstructor exp

  Source _ _ (TypedExp exp _) ->
    expUsesDictionaryConstructor exp

  Source _ _ (NamedTypedExp _ exp _) ->
    expUsesDictionaryConstructor exp

  Source _ _ (ListConstructor listItems) ->
    any
      (\case
        Source _ _ (ListItem exp) ->
          expUsesDictionaryConstructor exp

        Source _ _ (ListSpread exp) ->
          expUsesDictionaryConstructor exp
      )
      listItems

  Source _ _ (TupleConstructor exps) ->
    any expUsesDictionaryConstructor exps

  Source _ _ (Pipe exps) ->
    any expUsesDictionaryConstructor exps

  Source _ _ (JsxTag _ props children) ->
    any
      (\case Source _ _ (JsxProp _ exp) -> expUsesDictionaryConstructor exp)
      props
    || any
        (\case
          JsxChild exp ->
            expUsesDictionaryConstructor exp

          JsxExpChild exp ->
            expUsesDictionaryConstructor exp

          JsxSpreadChild exp ->
            expUsesDictionaryConstructor exp
        )
        children

  Source _ _ (JsxAutoClosedTag _ props) ->
    any
      (\case Source _ _ (JsxProp _ exp) -> expUsesDictionaryConstructor exp)
      props

  Source _ _ (Parenthesized _ exp _) ->
    expUsesDictionaryConstructor exp

  _ -> False
