{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Compile.Compile where

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( isInfixOf
                                                , sort
                                                , find
                                                , intercalate
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
import           Debug.Trace                    ( trace )
import           Text.Show.Pretty               ( ppShow )
import           Compile.JSInternals
import           Target
import           Compile.Utils



hpWrapLine :: Bool -> FilePath -> Int -> String -> String
hpWrapLine coverage astPath line compiled = if coverage
  then
    "__hpLineWrap('" <> astPath <> "', " <> show line <> ", " <> compiled <> ")"
  else compiled

data CompilationConfig
  = CompilationConfig
      { ccrootPath       :: FilePath
      , ccastPath        :: FilePath
      , ccentrypointPath :: FilePath
      , ccoutputPath     :: FilePath
      , cccoverage       :: Bool
      , ccoptimize       :: Bool
      , cctarget         :: Target
      }

class Compilable a where
  compile :: CompilationConfig -> a -> String

instance Compilable Exp where
  compile config e@(Optimized _ area@(Area (Loc _ l _) _) exp) =
    let
      astPath   = ccastPath config
      coverage  = cccoverage config
      optimized = ccoptimize config
    in
      case exp of
        LNum  v -> hpWrapLine coverage astPath l v
        LStr  v -> hpWrapLine coverage astPath l $ "`" <> v <> "`"
        LBool v -> hpWrapLine coverage astPath l v
        LUnit   -> hpWrapLine coverage
                              astPath
                              l
                              "({ __constructor: \"Unit\", __args: [] })"

        TemplateString exps ->
          let parts = foldl
                (\full e -> case e of
                  Opt.Optimized _ _ (LStr v) -> full <> v

                  _ -> full <> "${" <> compile config e <> "}"
                )
                ""
                exps
          in  "`" <> parts <> "`"

        App abs arg final -> case abs of
          Optimized _ _ (App (Optimized _ _ (Var "++")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " + "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg')
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "+")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " + "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "-")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " - "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "*")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " * "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "/")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " / "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "%")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " % "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "==")) arg' _) ->
            eqFnName optimized
              <> "("
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg')
                            (compile config arg')
              <> ", "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
              <> ")"
          Optimized _ _ (App (Optimized _ _ (Var "&&")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " && "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "||")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " || "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var ">")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " > "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "<")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " < "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var ">=")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " >= "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)
          Optimized _ _ (App (Optimized _ _ (Var "<=")) arg' _) ->
            hpWrapLine coverage
                       astPath
                       (getStartLine arg')
                       (compile config arg')
              <> " <= "
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg)

          Optimized _ _ (App (Optimized _ _ (Var "|>")) arg' _) ->
            hpWrapLine coverage astPath (getStartLine arg') (compile config arg)
              <> "("
              <> hpWrapLine coverage
                            astPath
                            (getStartLine arg)
                            (compile config arg')
              <> ")"

          _ -> compileApp [] [] abs arg final
         where
          compileApp :: [Bool] -> [String] -> Exp -> Exp -> Bool -> String
          compileApp prevFinals prevArgs abs arg final =
            let args =
                    [ hpWrapLine coverage astPath (getStartLine arg)
                        $ compile config arg
                      ]
                      <> prevArgs
                finals = [final] <> prevFinals
                next   = case abs of
                  (Optimized _ _ (App abs' arg' final')) ->
                    compileApp finals args abs' arg' final'
                  _ ->
                    let finalized = zip args finals
                    in  buildAbs config abs arg
                          <> "("
                          <> buildParams finalized
                          <> ")"
            in  hpWrapLine coverage astPath (getStartLine abs) next

          buildAbs :: CompilationConfig -> Exp -> Exp -> String
          buildAbs config abs arg = compile config abs

          buildParams :: [(String, Bool)] -> String
          buildParams [] = ""
          buildParams ((arg, final) : args) =
            if final && null args then arg else arg <> ")(" <> buildParams args


        If cond truthy falsy ->
          "("
            <> compile config cond
            <> " ? "
            <> compile config truthy
            <> " : "
            <> compile config falsy
            <> ")"

        Abs param body -> compileAbs Nothing param body
         where
          compileAbs :: Maybe [Exp] -> Name -> [Exp] -> String
          compileAbs parent param body =
            let start = case parent of
                  Just _  -> ", " <> param
                  Nothing -> curryFnName optimized <> "((" <> param
                next = case head body of
                  (Optimized _ _ (Abs param' body')) ->
                    compileAbs (Just body) param' body'
                  _ -> ") => " <> compileBody body <> ")"
            in  start <> next

          compileBody :: [Exp] -> String
          compileBody [exp] = compile config exp
          compileBody exps  = "{\n" <> compileBody' exps <> "}"

          compileBody' :: [Exp] -> String
          compileBody' [exp] = case exp of
            (Optimized _ _ (JSExp _)) -> compile config exp
            _ -> "    return " <> compile config exp <> ";\n"
          compileBody' (exp : es) =
            "    " <> compile config exp <> ";\n" <> compileBody' es

        Var name -> if '.' `elem` name || name == "!" || not coverage
          then name
          else hpWrapLine coverage astPath l name

        NamespaceAccess name ->
          if '.' `elem` name || name == "!" || not coverage
            then name
            else hpWrapLine coverage astPath l name


        Placeholder (ClassRef cls _ call var, ts) exp' -> insertPlaceholderArgs
          ""
          e

         where
          insertPlaceholderArgs :: String -> Exp -> String
          insertPlaceholderArgs prev exp'' = case exp'' of
            Opt.Optimized _ _ (Placeholder (ClassRef cls ps call var, ts) exp''')
              -> let dict  = generateRecordName optimized cls ts var
                     dict' = partiallyApplySubDicts dict ps
                 in  insertPlaceholderArgs (prev <> "(" <> dict' <> ")") exp'''

            _ -> compile config exp'' <> prev

          partiallyApplySubDicts :: String -> [ClassRefPred] -> String
          partiallyApplySubDicts dict ps = if not (null ps)
            then
              let dicts =
                    "["
                      <> intercalate
                           ", "
                           (   (\(CRPNode cls' ts var subdicts) ->
                                 partiallyApplySubDicts
                                   (generateRecordName optimized cls' ts var)
                                   subdicts
                               )
                           <$> ps
                           )
                      <> "]"
              in  applyDictsFnName optimized
                  <> "("
                  <> dict
                  <> ", "
                  <> dicts
                  <> ")"
            else dict

          getTypeName :: Type -> String
          getTypeName t = case t of
            TCon (TC n _)          -> n
            TApp (TCon (TC n _)) _ -> n
            TApp (TApp (TCon (TC n _)) _) _ ->
              if n == "(,)" then "Tuple_2" else n
            TApp (TApp (TApp (TCon (TC n _)) _) _) _ ->
              if n == "(,,)" then "Tuple_3" else n
            _ -> ppShow t


        Placeholder (MethodRef cls method var, ts) (Opt.Optimized _ _ (Var name))
          -> generateRecordName optimized cls ts var <> "." <> method

        -- Build ABS for coverage
        Assignment name abs@(Optimized _ _ (Abs param body)) -> if coverage
          then "const " <> name <> " = " <> compileAbs Nothing param body <> ""
          else
            "const "
            <> name
            <> " = "
            <> compileAssignmentWithPlaceholder config abs
         where
          compileAbs :: Maybe [Exp] -> Name -> [Exp] -> String
          compileAbs parent param body =
            let start = case parent of
                  Just _ -> ", " <> param
                  Nothing ->
                    let (Area (Loc _ line _) _) = area
                    in  "__hpFnWrap('"
                          <> astPath
                          <> "', "
                          <> show line
                          <> ", '"
                          <> name
                          <> "')("
                          <> curryFnName optimized
                          <> "(("
                          <> param
                next = case head body of
                  (Optimized _ _ (Abs param' body')) ->
                    compileAbs (Just body) param' body'
                  Optimized _ _ (JSExp _) ->
                    ") => " <> compile config (head body) <> "))"
                  _ -> ") => " <> compileBody body <> "))"
            in  start <> next

          compileBody :: [Exp] -> String
          compileBody [exp] = compile config exp
          compileBody exps  = "{" <> compileBody' exps <> "}"

          compileBody' :: [Exp] -> String
          compileBody' [exp] = case exp of
            (Optimized _ _ (JSExp _)) -> compile config exp
            _ -> "return " <> compile config exp <> ";\n"
          compileBody' (exp : es) =
            compile config exp <> ";\n" <> compileBody' es

        Assignment name exp ->
          "const "
            <> name
            <> " = "
            <> compileAssignmentWithPlaceholder config exp

        TypedExp exp _ -> compile config exp

        Export ass@(Optimized _ _ (Assignment name exp)) -> case exp of
          Optimized _ _ (Abs _ _) -> "export " <> compile config ass
          _ ->
            "export const "
              <> name
              <> " = "
              <> compileAssignmentWithPlaceholder config exp

        Record fields ->
          let fs = intercalate "," $ compileField <$> fields
          in  "({" <> fs <> " })"
         where
          compileField :: Field -> String
          compileField field = case field of
            Field (name, exp) -> " " <> name <> ": " <> hpWrapLine
              coverage
              astPath
              (getStartLine exp)
              (compile config exp)
            FieldSpread exp -> " ..." <> hpWrapLine coverage
                                                    astPath
                                                    (getStartLine exp)
                                                    (compile config exp)

        FieldAccess record field ->
          hpWrapLine coverage astPath (getStartLine record)
            $  compile config record
            <> compile config field

        JSExp content -> content

        ListConstructor elems ->
          "([" <> intercalate ", " (compileListItem <$> elems) <> "])"
         where
          compileListItem :: ListItem -> String
          compileListItem li = case li of
            ListItem   exp -> compile config exp
            ListSpread exp -> " ..." <> compile config exp

        TupleConstructor elems ->
          "([" <> intercalate ", " (compile config <$> elems) <> "])"

        Where exp (first : cs) ->
          "((__x__) => {\n  "
            <> compileIs first
            <> concat (("  else " ++) . compileIs <$> cs)
            <> "  else { console.log('non exhaustive patterns for value: ', __x__.toString()); \nthrow 'non exhaustive patterns!'; }\n"
            -- TODO: Add an else for undefined patterns error and make it throw.
            <> "})("
            <> compile config exp
            <> ")"
         where
          compileListOrTuplePattern :: String -> [Pattern] -> String
          compileListOrTuplePattern scope [] = scope <> ".length === 0"
          compileListOrTuplePattern scope items =
            scope
              <> ".length "
              <> lengthComparator items
              <> " "
              <> show (length items)
              <> " && "
              <> intercalate
                   " && "
                   (   (\(item, i) ->
                         compilePattern (scope <> "[" <> show i <> "]") item
                       )
                   <$> zip items [0 ..]
                   )
           where
            lengthComparator :: [Pattern] -> String
            lengthComparator pats = if containsSpread pats then ">=" else "==="
            containsSpread :: [Pattern] -> Bool
            containsSpread pats =
              let isSpread = \case
                    PSpread _ -> True
                    _         -> False
              in  case find isSpread pats of
                    Just _  -> True
                    Nothing -> False

          compilePattern :: String -> Pattern -> String
          compilePattern _     (PVar _)  = "true"
          compilePattern _     PAny      = "true"
          compilePattern scope (PNum  n) = scope <> " === " <> n
          compilePattern scope (PStr  n) = scope <> " === \"" <> n <> "\""
          compilePattern scope (PBool n) = scope <> " === " <> n
          compilePattern scope (PCon n)
            | n == "String"  = "typeof " <> scope <> " === \"string\""
            | n == "Boolean" = "typeof " <> scope <> " === \"boolean\""
            | n == "Number"  = "typeof " <> scope <> " === \"number\""
            | otherwise      = ""
          compilePattern scope (PCtor n []) =
            scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\""
          compilePattern scope (PCtor n ps) =
            scope
              <> ".__constructor === "
              <> "\""
              <> removeNamespace n
              <> "\""
              <> if not (null args) then " && " <> args else ""
           where
            args =
              intercalate " && "
                $   filter (not . null)
                $   compileCtorArg scope n
                <$> zip [0 ..] ps
          compilePattern scope (PRecord m) =
            intercalate " && " $ filter (not . null) $ M.elems $ M.mapWithKey
              (compileRecord scope)
              m

          compilePattern scope (PSpread pat) = compilePattern scope pat
          compilePattern scope (PList items) =
            compileListOrTuplePattern scope items
          compilePattern scope (PTuple items) =
            compileListOrTuplePattern scope items

          compilePattern _ _ = ""


          compileIs :: Is -> String
          compileIs (Optimized _ (Area (Loc _ l _) _) (Is pat exp)) =
            "if ("
              <> (if coverage
                   then
                     "__hp('"
                     <> astPath
                     <> "', 'line', "
                     <> show l
                     <> ", "
                     <> show l
                     <> ") || "
                   else ""
                 )
              <> compilePattern "__x__" pat
              <> ") {\n"
              <> buildVars "__x__" pat
              <> "    return "
              <> compile config exp
              <> ";\n  }\n"

          buildVars :: String -> Pattern -> String
          buildVars v p = case p of
            PRecord fields ->
              "    const { "
                <> intercalate
                     ", "
                     ( filter (not . null)
                     . ((snd <$>) . reverse . sort . M.toList)
                     $ M.mapWithKey buildFieldVar fields
                     )
                <> " } = "
                <> v
                <> ";\n"
             where
              buildFieldVar :: String -> Pattern -> String
              buildFieldVar name pat = case pat of
                PSpread (PVar n) -> "..." <> n
                PVar    n        -> if null name then n else name <> ": " <> n
                PRecord fields ->
                  name
                    <> ": { "
                    <> intercalate
                         ", "
                         ( filter (not . null)
                         . ((snd <$>) . reverse . sort . M.toList)
                         $ M.mapWithKey buildFieldVar fields
                         )
                    <> " }"
                PCtor _ args -> if null name
                  then
                    "{ __args: ["
                    <> intercalate ", " (buildFieldVar "" <$> args)
                    <> "] }"
                  else
                    name
                    <> ": "
                    <> "{ __args: ["
                    <> intercalate
                         ", "
                         (   (\(i, arg) -> buildFieldVar "" arg)
                         <$> zip [0 ..] args
                         )
                    <> "] }"
                _ -> ""
            PList  items -> buildTupleOrListVars v items
            PTuple items -> buildTupleOrListVars v items

            PCtor _ ps ->
              concat
                $   (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "]") p)
                <$> zip [0 ..] ps
            PVar n -> "    const " <> n <> " = " <> v <> ";\n"

            _      -> ""

          buildTupleOrListVars :: String -> [Pattern] -> String
          buildTupleOrListVars scope items =
            let itemsStr = buildListVar <$> items
            in  "    const ["
                  <> intercalate "," itemsStr
                  <> "] = "
                  <> scope
                  <> ";\n"
           where
            buildListVar :: Pattern -> String
            buildListVar pat = case pat of
              PSpread (PVar n) -> "..." <> n
              PVar    n        -> n
              PCtor _ args ->
                let built = intercalate ", " $ buildListVar <$> args
                in  "{ __args: [" <> built <> "]}"
              PList pats ->
                "[" <> intercalate ", " (buildListVar <$> pats) <> "]"
              PTuple pats ->
                "[" <> intercalate ", " (buildListVar <$> pats) <> "]"
              _ -> ""

          compileRecord :: String -> Name -> Pattern -> String
          compileRecord scope n p = compilePattern (scope <> "." <> n) p

          compileCtorArg :: String -> String -> (Int, Pattern) -> String
          compileCtorArg scope _ (x, p) =
            compilePattern (scope <> ".__args[" <> show x <> "]") p

        _ -> "/* Not implemented: " <> ppShow exp <> "*/\n"


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable TypeDecl where
  compile _ Alias{}                      = ""
  compile _ ADT { adtconstructors = [] } = ""
  compile config adt =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>) "" (addExport exported . compile config <$> ctors)
   where
    addExport :: Bool -> String -> String
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  compile config (Constructor cname cparams) =
    let coverage  = cccoverage config
        optimized = ccoptimize config
    in  case cparams of
          [] ->
            "const " <> cname <> " = " <> compileBody cname cparams <> ";\n"
          _ ->
            "const "
              <> cname
              <> " = "
              <> curryFnName optimized
              <> "("
              <> compileParams cparams
              <> " => "
              <> compileBody cname cparams
              <> ");\n"
   where
    compileParams n =
      let argNames = (: []) <$> take (length n) ['a' ..]
      in  "(" <> intercalate ", " argNames <> ")"

    compileBody n a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          args     = argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"


compileImport :: CompilationConfig -> Import -> String
compileImport config imp = case imp of
  NamedImport names _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import { " <> compileNames names <> " } from \"" <> importPath <> "\""
   where
    compileNames names =
      if null names then "" else (init . init . concat) $ (++ ", ") <$> names
  DefaultImport alias _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import " <> alias <> " from \"" <> importPath <> "\""


buildImportPath :: CompilationConfig -> FilePath -> FilePath
buildImportPath config absPath =
  let outputPath = ccoutputPath config
      rootPath   = ccrootPath config
      astPath    = ccastPath config
      destPath   = dropFileName $ computeTargetPath outputPath rootPath astPath
      depPath    = computeTargetPath outputPath rootPath absPath
  in  cleanRelativePath $ replaceExtension
        (joinPath ["./", makeRelativeEx destPath depPath])
        ".mjs"


updateASTPath :: FilePath -> CompilationConfig -> CompilationConfig
updateASTPath astPath config = config { ccastPath = astPath }

instance Compilable Opt.Interface where
  compile config interface = case interface of
    Opt.Interface name _ _ _ ->
      getGlobalForTarget (cctarget config) <> "." <> name <> " = {};\n"

instance Compilable Opt.Instance where
  compile config inst = case inst of
    Opt.Instance interface _ typings dict -> -- "INST" <> interface
      interface
        <> "['"
        <> typings
        <> "']"
        <> " = {\n"
        <> intercalate ",\n"
                       (uncurry compileMethod <$> M.toList (M.map fst dict))
        <> "\n};\n"
   where
    compileMethod :: Name -> Exp -> String
    compileMethod n exp =
      "  " <> n <> ": " <> compileAssignmentWithPlaceholder config exp


compileAssignmentWithPlaceholder :: CompilationConfig -> Exp -> String
compileAssignmentWithPlaceholder config fullExp@(Opt.Optimized _ _ exp) =
  case exp of
    Placeholder (ClassRef cls _ call var, ts) e -> if not call
      then
        let dict = generateRecordName (ccoptimize config) cls ts var
        in  "("
            <> dict
            <> ") => ("
            <> compileAssignmentWithPlaceholder config e
            <> ")"
      else compile config fullExp

    _ -> compile config fullExp

generateRecordName :: Bool -> String -> String -> Bool -> String
generateRecordName optimized cls ts var = if var
  then if optimized then cls <> ts else cls <> "_" <> ts
  else cls <> "." <> ts

instance Compilable AST where
  compile config ast =
    let
      entrypointPath     = ccentrypointPath config
      coverage           = cccoverage config
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
        x  -> foldr1 (<>) (compile configWithASTPath <$> x)
      compiledInstances = case instances of
        [] -> ""
        x  -> foldr1 (<>) (compile configWithASTPath <$> x)
      compiledAdts = case typeDecls of
        [] -> ""
        x  -> foldr1 (<>) (compile configWithASTPath <$> x)
      compiledExps = case exps of
        [] -> ""
        x  -> foldr1 (<>) (terminate . compile configWithASTPath <$> x)
      compiledImports = case imports of
        [] -> ""
        x ->
          foldr1 (<>) (terminate . compileImport configWithASTPath <$> x)
            <> "\n"
      defaultExport = buildDefaultExport typeDecls exps
    in
      infoComment
      <> (if entrypointPath == astPath
           then "import {} from \"./__internals__.mjs\"\n"
           else ""
         )
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


buildDefaultExport :: [TypeDecl] -> [Exp] -> String
buildDefaultExport as es =
  let expExportNames = getExportName <$> filter isExport es
      adtExportNames = getConstructorName
        <$> concat (adtconstructors <$> filter isADTExport as)
      allDefaultExports = expExportNames <> adtExportNames
  in  case allDefaultExports of
        []      -> "export default {};\n"
        exports -> "export default { " <> intercalate ", " exports <> " };\n"

 where
  isADTExport :: TypeDecl -> Bool
  isADTExport ADT { adtexported } = adtexported
  isADTExport _                   = False

  isExport :: Exp -> Bool
  isExport a = case a of
    (Optimized _ _ (Export _)) -> True
    (Optimized _ _ (TypedExp (Optimized _ _ (Export _)) _)) -> True

    _ -> False

  getExportName :: Exp -> String
  getExportName (Optimized _ _ (Export (Optimized _ _ (Assignment n _)))) = n
  getExportName (Optimized _ _ (TypedExp (Optimized _ _ (Export (Optimized _ _ (Assignment n _)))) _))
    = n

  getConstructorName :: Constructor -> String
  getConstructorName (Constructor cname _) = cname
