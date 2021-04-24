{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Compile.Javascript where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( isInfixOf
                                                , sort
                                                , find
                                                , intercalate
                                                , foldl'
                                                )

import           AST.Optimized                 as Opt
import           Utils.Path                     ( cleanRelativePath
                                                , computeTargetPath
                                                , makeRelativeEx
                                                )
import           System.FilePath                ( replaceExtension
                                                , dropFileName
                                                , joinPath
                                                )
import           Explain.Location
import           Infer.Type
import           Compile.JSInternals
import           Target
import           Compile.Utils
import           Text.Show.Pretty               ( ppShow )
import           Debug.Trace


newtype Env = Env { varsInScope :: S.Set String }

initialEnv :: Env
initialEnv = Env { varsInScope = S.empty }


hpWrapLine :: Bool -> FilePath -> Int -> String -> String
hpWrapLine coverage astPath line compiled =
  if coverage then "__hpLineWrap('" <> astPath <> "', " <> show line <> ", " <> compiled <> ")" else compiled

data CompilationConfig
  = CompilationConfig
      { ccrootPath       :: FilePath
      , ccastPath        :: FilePath
      , ccentrypointPath :: FilePath
      , ccoutputPath     :: FilePath
      , cccoverage       :: Bool
      , ccoptimize       :: Bool
      , cctarget         :: Target
      , ccinternalsPath  :: FilePath
      }

class Compilable a where
  compile :: Env -> CompilationConfig -> a -> String

instance Compilable Exp where
  compile env config e@(Optimized expType area@(Area (Loc _ l _) _) exp) =
    let
      astPath   = ccastPath config
      coverage  = cccoverage config
      optimized = ccoptimize config
    in
      case exp of
        LNum  v -> hpWrapLine coverage astPath l v
        LStr  v -> hpWrapLine coverage astPath l v
        LBool v -> hpWrapLine coverage astPath l v
        LUnit   -> hpWrapLine coverage astPath l "({ __constructor: \"Unit\", __args: [] })"

        TemplateString exps ->
          let parts = foldl'
                (\full e -> case e of
                  Opt.Optimized _ _ (LStr v) -> full <> v

                  _                          -> full <> "${" <> compile env config e <> "}"
                )
                ""
                exps
          in  "`" <> parts <> "`"

        App abs arg final -> case abs of
          Optimized _ _ (App (Optimized _ _ (Var "++")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " + " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg')
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "+")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " + " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (Var "unary-minus") ->
            "-" <> hpWrapLine coverage astPath (getStartLine arg) (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "-")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " - " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "*")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " * " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "/")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " / " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "%")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " % " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "==")) arg' _) ->
            eqFnName optimized
              <> "("
              <> hpWrapLine coverage astPath (getStartLine arg') (compile env config arg')
              <> ", "
              <> hpWrapLine coverage astPath (getStartLine arg) (compile env config arg)
              <> ")"
          Optimized _ _ (App (Optimized _ _ (Var "!=")) arg' _) ->
            "!"
              <> eqFnName optimized
              <> "("
              <> hpWrapLine coverage astPath (getStartLine arg') (compile env config arg')
              <> ", "
              <> hpWrapLine coverage astPath (getStartLine arg) (compile env config arg)
              <> ")"
          Optimized _ _ (App (Optimized _ _ (Var "&&")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " && " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "||")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " || " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var ">")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " > " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "<")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " < " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var ">=")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " >= " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)
          Optimized _ _ (App (Optimized _ _ (Var "<=")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg') <> " <= " <> hpWrapLine
              coverage
              astPath
              (getStartLine arg)
              (compile env config arg)

          Optimized _ _ (App (Optimized _ _ (Var "|>")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile env config arg)
              <> "("
              <> hpWrapLine coverage astPath (getStartLine arg) (compile env config arg')
              <> ")"
          _ -> compileApp [] [] abs arg final
         where
          compileApp :: [Bool] -> [String] -> Exp -> Exp -> Bool -> String
          compileApp prevFinals prevArgs abs arg final =
            let args   = [hpWrapLine coverage astPath (getStartLine arg) $ compile env config arg] <> prevArgs
                finals = [final] <> prevFinals
                next   = case abs of
                  (Optimized _ _ (App abs' arg' final')) -> compileApp finals args abs' arg' final'
                  _ ->
                    let finalized = zip args finals in buildAbs config abs arg <> "(" <> buildParams finalized <> ")"
            in  hpWrapLine coverage astPath (getStartLine abs) next

          buildAbs :: CompilationConfig -> Exp -> Exp -> String
          buildAbs config abs arg = compile env config abs

          buildParams :: [(String, Bool)] -> String
          buildParams []                    = ""
          buildParams ((arg, final) : args) = --if final && null args then arg else arg <> ")(" <> buildParams args
                                              if final
            then if null args then arg else arg <> ")(" <> buildParams args
            else arg <> ", " <> buildParams args


        If cond truthy falsy ->
          "("
            <> compile env config cond
            <> " ? "
            <> compile env config truthy
            <> " : "
            <> compile env config falsy
            <> ")"

        Abs param body -> compileAbs Nothing param body
         where
          compileAbs :: Maybe [Exp] -> Name -> [Exp] -> String
          compileAbs parent param body =
            let start = case parent of
                  Just _  -> ", " <> param
                  Nothing -> curryFnName optimized <> "((" <> param
                next = case head body of
                  (Optimized _ _ (Abs param' body')) -> compileAbs (Just body) param' body'
                  _ -> ") => " <> compileBody env body <> ")"
            in  start <> next

          compileBody :: Env -> [Exp] -> String
          compileBody _   [exp] = compile env config exp
          compileBody env exps  = "{\n" <> compileBody' env exps <> "}"

          compileBody' :: Env -> [Exp] -> String
          compileBody' _ [exp] = case exp of
            (Optimized _ _ (JSExp _)) -> compile env config exp
            _                         -> "    return " <> compile env config exp <> ";\n"
          compileBody' e (exp : es) = case exp of
            Opt.Optimized _ _ (Opt.Assignment name _) ->
              let nextEnv = e { varsInScope = S.insert name (varsInScope e) }
              in  "    " <> compile e config exp <> ";\n" <> compileBody' nextEnv es

            _ -> "    " <> compile e config exp <> ";\n" <> compileBody' e es

        Var ('.' : name) -> "(__R__ => __R__." <> name <> ")"


        Var name -> if name == "!" || not coverage then name else hpWrapLine coverage astPath l name

        Placeholder (ClassRef cls _ call var, ts) exp' -> insertPlaceholderArgs "" e

         where
          insertPlaceholderArgs :: String -> Exp -> String
          insertPlaceholderArgs prev exp'' = case exp'' of
            Opt.Optimized _ _ (Placeholder (ClassRef cls ps call var, ts) exp''') ->
              let dict  = generateRecordName optimized cls ts var
                  dict' = partiallyApplySubDicts dict ps
              in  insertPlaceholderArgs (prev <> "(" <> dict' <> ")") exp'''

            _ -> compile env config exp'' <> prev

          partiallyApplySubDicts :: String -> [ClassRefPred] -> String
          partiallyApplySubDicts dict ps = if not (null ps)
            then
              let
                dicts =
                  "["
                    <> intercalate
                         ", "
                         (   (\(CRPNode cls' ts var subdicts) ->
                               partiallyApplySubDicts (generateRecordName optimized cls' ts var) subdicts
                             )
                         <$> ps
                         )
                    <> "]"
              in  applyDictsFnName optimized <> "(" <> dict <> ", " <> dicts <> ")"
            else dict

          getTypeName :: Type -> String
          getTypeName t = case t of
            TCon (TC n _) _ -> n
            TApp (TCon (TC n _) _) _ -> n
            TApp (TApp (TCon (TC n _) _) _) _ -> if n == "(,)" then "Tuple_2" else n
            TApp (TApp (TApp (TCon (TC n _) _) _) _) _ -> if n == "(,,)" then "Tuple_3" else n
            _ -> ppShow t


        Placeholder (MethodRef cls method var, ts) (Opt.Optimized _ _ (Var name)) ->
          let compiled = generateRecordName optimized cls ts var <> "." <> method
          in  if not coverage then compiled else hpWrapLine coverage astPath l compiled

        Assignment name exp ->
          let (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
              content'                       = if coverage && isFunctionType expType
                then "__hpFnWrap('" <> astPath <> "', " <> show l <> ", '" <> name <> "')(" <> content <> ")"
                else content
              needsModifier = not $ name `elem` varsInScope env
          in  if not (null dicts)
                then
                  (if needsModifier then "let " else "")
                  <> name
                  <> " = "
                  <> placeholders
                  <> "{"
                  <> "\n  "
                  <> intercalate "\n  " ((\dict -> getGlobalForTarget (cctarget config) <> "." <> dict <> " = " <> dict) <$> dicts)
                  <> "\n\n"
                  <> "  return "
                  <> name
                  <> "__ND__()"
                  <> "\n};\n"
                  <> "let "
                  <> name
                  <> "__ND__"
                  <> " = "
                  <> onceFnName optimized
                  <> "(() => "
                  <> content'
                  <> ")"
                else (if needsModifier then "let " else "") <> name <> " = " <> content'

        TypedExp exp _    -> compile env config exp

        Export     ass    -> "export " <> compile env config ass

        NameExport name   -> "export { " <> name <> " }"

        Record     fields -> let fs = intercalate "," $ compileField <$> fields in "({" <> fs <> " })"
         where
          compileField :: Field -> String
          compileField (Optimized _ _ field) = case field of
            Field (name, exp) ->
              " " <> name <> ": " <> hpWrapLine coverage astPath (getStartLine exp) (compile env config exp)
            FieldSpread exp -> " ..." <> hpWrapLine coverage astPath (getStartLine exp) (compile env config exp)

        Access record (Optimized _ _ (Var name)) ->
          hpWrapLine coverage astPath (getStartLine record) $ compile env config record <> name

        JSExp           content -> content

        ListConstructor elems   -> "([" <> intercalate ", " (compileListItem <$> elems) <> "])"
         where
          compileListItem :: ListItem -> String
          compileListItem (Optimized _ _ li) = case li of
            ListItem   exp -> compile env config exp
            ListSpread exp -> " ..." <> compile env config exp

        TupleConstructor elems -> "([" <> intercalate ", " (compile env config <$> elems) <> "])"

        Where exp (first : cs) ->
          "((__x__) => {\n  "
            <> compileIs first
            <> concat (("  else " ++) . compileIs <$> cs)
            <> "  else {\n    console.log('non exhaustive patterns for value: ', __x__.toString()); \n    throw 'non exhaustive patterns!';\n  }\n"
            <> "})("
            <> compile env config exp
            <> ")"
         where
          compileListOrTuplePattern :: String -> [Pattern] -> String
          compileListOrTuplePattern scope [] = scope <> ".length === 0"
          compileListOrTuplePattern scope items =
            scope <> ".length " <> lengthComparator items <> " " <> show (length items) <> " && " <> intercalate
              " && "
              ((\(item, i) -> compilePattern (scope <> "[" <> show i <> "]") item) <$> zip items [0 ..])
           where
            lengthComparator :: [Pattern] -> String
            lengthComparator pats = if containsSpread pats then ">=" else "==="
            containsSpread :: [Pattern] -> Bool
            containsSpread pats =
              let isSpread = \case
                    Optimized _ _ (PSpread _) -> True
                    _                         -> False
              in  case find isSpread pats of
                    Just _  -> True
                    Nothing -> False

          compilePattern :: String -> Pattern -> String
          compilePattern scope (Optimized _ _ pat) = case pat of
            PVar _  -> "true"
            PAny    -> "true"
            PNum  n -> scope <> " === " <> n
            PStr  n -> scope <> " === " <> n
            PBool n -> scope <> " === " <> n
            PCon n | n == "String"  -> "typeof " <> scope <> " === \"string\""
                   | n == "Boolean" -> "typeof " <> scope <> " === \"boolean\""
                   | n == "Number"  -> "typeof " <> scope <> " === \"number\""
                   | otherwise      -> ""
            PCtor n [] -> scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\""
            PCtor n ps ->
              let args = intercalate " && " $ filter (not . null) $ compileCtorArg scope n <$> zip [0 ..] ps
              in  scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\"" <> if not (null args)
                    then " && " <> args
                    else ""
            PRecord m     -> intercalate " && " $ filter (not . null) $ M.elems $ M.mapWithKey (compileRecord scope) m

            PSpread pat   -> compilePattern scope pat
            PList   items -> compileListOrTuplePattern scope items
            PTuple  items -> compileListOrTuplePattern scope items


          compileIs :: Is -> String
          compileIs (Optimized _ (Area (Loc _ l _) _) (Is pat exp)) =
            "if ("
              <> (if coverage then "__hp('" <> astPath <> "', 'line', " <> show l <> ", " <> show l <> ") || " else "")
              <> compilePattern "__x__" pat
              <> ") {\n"
              <> buildVars "__x__" pat
              <> "    return "
              <> compile env config exp
              <> ";\n  }\n"

          buildVars :: String -> Pattern -> String
          buildVars v (Optimized _ _ pat) = case pat of
            PRecord fields ->
              "    let { "
                <> intercalate
                     ", "
                     (filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields)
                <> " } = "
                <> v
                <> ";\n"
             where
              buildFieldVar :: String -> Pattern -> String
              buildFieldVar name (Optimized _ _ pat) = case pat of
                PSpread (Optimized _ _ (PVar n)) -> "..." <> n
                PVar    n                        -> if null name then n else name <> ": " <> n
                PRecord fields ->
                  name
                    <> ": { "
                    <> intercalate
                         ", "
                         (filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar
                                                                                                       fields
                         )
                    <> " }"
                PCtor _ args -> if null name
                  then "{ __args: [" <> intercalate ", " (buildFieldVar "" <$> args) <> "] }"
                  else
                    name
                    <> ": "
                    <> "{ __args: ["
                    <> intercalate ", " ((\(i, arg) -> buildFieldVar "" arg) <$> zip [0 ..] args)
                    <> "] }"
                _ -> ""
            PList  items -> buildTupleOrListVars v items
            PTuple items -> buildTupleOrListVars v items

            PCtor _ ps   -> concat $ (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "]") p) <$> zip [0 ..] ps
            PVar n       -> "    let " <> n <> " = " <> v <> ";\n"

            _            -> ""

          buildTupleOrListVars :: String -> [Pattern] -> String
          buildTupleOrListVars scope items =
            let itemsStr = buildListVar <$> items
            in  "    let [" <> intercalate "," itemsStr <> "] = " <> scope <> ";\n"
           where
            buildListVar :: Pattern -> String
            buildListVar (Optimized _ _ pat) = case pat of
              PSpread (Optimized _ _ (PVar n)) -> "..." <> n
              PVar    n    -> n
              PCtor _ args -> let built = intercalate ", " $ buildListVar <$> args in "{ __args: [" <> built <> "]}"
              PList  pats  -> "[" <> intercalate ", " (buildListVar <$> pats) <> "]"
              PTuple pats  -> "[" <> intercalate ", " (buildListVar <$> pats) <> "]"
              _            -> ""

          compileRecord :: String -> Name -> Pattern -> String
          compileRecord scope n p = compilePattern (scope <> "." <> n) p

          compileCtorArg :: String -> String -> (Int, Pattern) -> String
          compileCtorArg scope _ (x, p) = compilePattern (scope <> ".__args[" <> show x <> "]") p

        _ -> "/* Not implemented: " <> ppShow exp <> "*/\n"


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable TypeDecl where
  compile _ _ (Untyped _ Alias{}                     ) = ""
  compile _ _ (Untyped _ ADT { adtconstructors = [] }) = ""
  compile env config (Untyped _ adt) =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>) "" (addExport exported . compile env config <$> ctors)
   where
    addExport :: Bool -> String -> String
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  compile _ config (Untyped _ (Constructor cname cparams)) =
    let coverage  = cccoverage config
        optimized = ccoptimize config
    in  case cparams of
          [] -> "let " <> cname <> " = " <> compileBody cname cparams <> ";\n"
          _ ->
            "let "
              <> cname
              <> " = "
              <> curryFnName optimized
              <> "("
              <> compileParams cparams
              <> " => "
              <> compileBody cname cparams
              <> ");\n"
   where
    compileParams n = let argNames = (: []) <$> take (length n) ['a' ..] in "(" <> intercalate ", " argNames <> ")"

    compileBody n a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          args     = argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"


compileImport :: CompilationConfig -> Import -> String
compileImport config (Untyped _ imp) = case imp of
  NamedImport names _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import { " <> compileNames (Opt.getValue <$> names) <> " } from \"" <> importPath <> "\""
    where compileNames names = if null names then "" else (init . init . concat) $ (++ ", ") <$> names
  DefaultImport alias _ absPath ->
    let importPath = buildImportPath config absPath in "import " <> Opt.getValue alias <> " from \"" <> importPath <> "\""


buildImportPath :: CompilationConfig -> FilePath -> FilePath
buildImportPath config absPath =
  let outputPath = ccoutputPath config
      rootPath   = ccrootPath config
      astPath    = ccastPath config
      destPath   = dropFileName $ computeTargetPath outputPath rootPath astPath
      depPath    = computeTargetPath outputPath rootPath absPath
  in  cleanRelativePath $ replaceExtension (joinPath ["./", makeRelativeEx destPath depPath]) ".mjs"


updateASTPath :: FilePath -> CompilationConfig -> CompilationConfig
updateASTPath astPath config = config { ccastPath = astPath }

instance Compilable Opt.Interface where
  compile _ config (Untyped _ interface) = case interface of
    Opt.Interface name _ _ _ _ -> getGlobalForTarget (cctarget config) <> "." <> name <> " = {};\n"

instance Compilable Opt.Instance where
  compile env config (Untyped _ inst) = case inst of
    Opt.Instance interface _ typings dict -> interface <> "['" <> typings <> "'] = {};\n" <> concat
      (uncurry compileMethod <$> M.toList (M.map fst dict))
     where
      compileMethod :: Name -> Exp -> String
      compileMethod n (Opt.Optimized t (Area (Loc _ line _) _) (Opt.Assignment _ exp)) =
        let
          (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
          content'                       = if cccoverage config && isFunctionType t
            then "__hpFnWrap('" <> ccastPath config <> "', " <> show line <> ", '" <> n <> "')(" <> content <> ")"
            else content
          instRoot = interface <> "['" <> typings <> "']"
          compiledNDMethod =
            "let __"
              <> interface
              <> typings
              <> n
              <> " = "
              <> onceFnName (ccoptimize config)
              <> "(() => "
              <> content'
              <> ");\n"
          compiledMethod =
            instRoot
              <> "['"
              <> n
              <> "'] = "
              <> placeholders
              <> "{\n  "
              <> intercalate "\n  " ((\dict -> getGlobalForTarget (cctarget config) <> "." <> dict <> " = " <> dict) <$> dicts)
              <> "\n  return __"
              <> interface
              <> typings
              <> n
              <> "();\n};\n"
        in
          if not (null dicts)
            then compiledNDMethod <> compiledMethod
            else instRoot <> "['" <> n <> "'] = " <> content' <> ";\n"


compileAssignmentWithPlaceholder :: Env -> CompilationConfig -> Exp -> (String, [String], String)
compileAssignmentWithPlaceholder env config fullExp@(Opt.Optimized _ _ exp) = case exp of
  Placeholder (ClassRef cls _ call var, ts) e -> if not call
    then
      let dict                      = generateRecordName (ccoptimize config) cls ts var
          (phs, dicts, compiledExp) = compileAssignmentWithPlaceholder env config e
      in  ("(" <> dict <> ") => " <> phs, dicts <> [dict], compiledExp)
    else ("", [], compile env config fullExp)

  _ -> ("", [], compile env config fullExp)

generateRecordName :: Bool -> String -> String -> Bool -> String
generateRecordName optimized cls ts var =
  if var then if optimized then cls <> ts else cls <> "_" <> ts else cls <> "." <> ts

instance Compilable AST where
  compile env config ast =
    let entrypointPath     = ccentrypointPath config
        coverage           = cccoverage config
        internalsPath      = ccinternalsPath config
        exps               = aexps ast
        typeDecls          = atypedecls ast
        path               = apath ast
        imports            = aimports ast
        interfaces         = ainterfaces ast
        instances          = ainstances ast


        astPath            = fromMaybe "Unknown" path

        configWithASTPath  = updateASTPath astPath config

        infoComment        = "// file: " <> astPath <> "\n"

        compiledInterfaces = case interfaces of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledInstances = case instances of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledAdts = case typeDecls of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledExps = case exps of
          [] -> ""
          x  -> compileExps env configWithASTPath exps
          -- x  -> foldr1 (<>) (terminate . compile env configWithASTPath <$> x)
        compiledImports = case imports of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compileImport configWithASTPath <$> x) <> "\n"
        defaultExport = buildDefaultExport typeDecls exps
    in  infoComment
          <> (if entrypointPath == astPath then "import {} from \"" <> internalsPath <> "\"\n" else "")
          <> compiledImports
          <> compiledAdts
          <> compiledInterfaces
          <> compiledInstances
          <> compiledExps
          <> defaultExport
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"


compileExps :: Env -> CompilationConfig -> [Exp] -> String
compileExps env config [exp     ] = compile env config exp <> ";\n"
compileExps env config (exp : es) = case exp of
  Opt.Optimized _ _ (Opt.Assignment name _) ->
    let nextEnv = env { varsInScope = S.insert name (varsInScope env) }
    in  compile env config exp <> ";\n" <> compileExps nextEnv config es

  _ -> compile env config exp <> ";\n" <> compileExps env config es


buildDefaultExport :: [TypeDecl] -> [Exp] -> String
buildDefaultExport as es =
  let expExportNames    = getExportName <$> filter isExport es
      adtExportNames    = getConstructorName <$> concat (adtconstructors . getValue <$> filter isADTExport as)
      allDefaultExports = expExportNames <> adtExportNames
  in  case allDefaultExports of
        []      -> "export default {};\n"
        exports -> "export default { " <> intercalate ", " exports <> " };\n"

 where
  isADTExport :: TypeDecl -> Bool
  isADTExport (Untyped _ ADT { adtexported }) = adtexported
  isADTExport _                               = False

  isExport :: Exp -> Bool
  isExport a = case a of
    (Optimized _ _ (Export _)) -> True
    (Optimized _ _ (TypedExp (Optimized _ _ (Export _)) _)) -> True

    _ -> False

  getExportName :: Exp -> String
  getExportName (Optimized _ _ (Export (Optimized _ _ (Assignment n _))                             )) = n
  getExportName (Optimized _ _ (TypedExp (Optimized _ _ (Export (Optimized _ _ (Assignment n _)))) _)) = n

  getConstructorName :: Constructor -> String
  getConstructorName (Untyped _ (Constructor cname _)) = cname
