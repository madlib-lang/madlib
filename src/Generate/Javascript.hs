{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Generate.Javascript where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( sort
                                                , find
                                                , intercalate
                                                , foldl'
                                                )
import           Data.List.GroupBy              ( groupBy )

import           AST.Core                      as Core
import           Utils.Path                     ( cleanRelativePath
                                                , computeTargetPath
                                                , makeRelativeEx
                                                , convertWindowsSeparators
                                                )
import           System.FilePath                ( replaceExtension
                                                , dropFileName
                                                , joinPath
                                                )
import           Explain.Location
import           Infer.Type
import           Generate.JSInternals
import           Run.Target
import           Generate.Utils
import           Text.Show.Pretty               ( ppShow )
import Distribution.Types.Lens (_Impl)


newtype Env = Env { varsInScope :: S.Set String } deriving(Eq, Show)

initialEnv :: Env
initialEnv = Env { varsInScope = S.empty }

allowedJSNames :: [String]
allowedJSNames = ["delete", "class", "while", "for", "case", "switch", "try", "length", "var", "default"]

generateSafeName :: String -> String
generateSafeName n = if n `elem` allowedJSNames then "_$_" <> n <> "_$_" else n

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
  compile _ _ Untyped{} = ""
  compile env config e@(Typed (_ :=> expType) area@(Area (Loc _ l _) _) _ exp) =
    let
      astPath   = ccastPath config
      coverage  = cccoverage config
      optimized = ccoptimize config
    in
      case exp of
        Literal (LNum v) ->
          hpWrapLine coverage astPath l v

        Literal (LFloat v) ->
          hpWrapLine coverage astPath l v

        Literal (LStr (leading : v)) | leading == '"' || leading == '\'' ->
          if null v then
            hpWrapLine coverage astPath l "``"
          else
            hpWrapLine coverage astPath l ("`" <> init v <> "`")

        Literal (LStr v) ->
          hpWrapLine coverage astPath l ("`" <> v <> "`")

        Literal (LBool v) ->
          hpWrapLine coverage astPath l v

        Literal LUnit ->
          hpWrapLine coverage astPath l "({ __constructor: \"Unit\", __args: [] })"

        Call (Typed _ _ _ (Var "++")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " + "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "unary-minus")) [arg] ->
          "-" <> hpWrapLine coverage astPath (getStartLine arg) (compile env config arg)

        Call (Typed _ _ _ (Var "+")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " + "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "-")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " - "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "*")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " * "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "/")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " / "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "%")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " % "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "==")) [left, right] ->
          eqFnName optimized
          <> "("
          <> hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> ", "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)
          <> ")"

        Call (Typed _ _ _ (Var "!=")) [left, right] ->
          "!"
          <> eqFnName optimized
          <> "("
          <> hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> ", "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)
          <> ")"

        Call (Typed _ _ _ (Var "!=")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " != "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "&&")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " && "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "||")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " || "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "|")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " | "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "&")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " & "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "^")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " ^ "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "~")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " ~ "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "<<")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " << "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">>")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " >> "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">>>")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " >>> "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " > "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "<")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " < "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">=")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " >= "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "<=")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " <= "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "|>")) [left, right] ->
          hpWrapLine coverage astPath (getStartLine right) (compile env config right)
            <> "("
            <> hpWrapLine coverage astPath (getStartLine left) (compile env config left)
            <> ")"

        Call fn args ->
          let compiledArgs = (<> ")") . ("(" <>) . compile env config <$> args
          in  compile env config fn <> concat compiledArgs

        If cond truthy falsy ->
          "("
            <> compile env config cond
            <> " ? "
            <> compile env config truthy
            <> " : "
            <> compile env config falsy
            <> ")"

        Definition params body -> compileAbs Nothing params body
         where
          compileAbs :: Maybe [Exp] -> [Name] -> [Exp] -> String
          compileAbs parent params body =
            let params' = intercalate " => " params
            in  "("
                <> params'
                <> " => "
                <> compileBody env{ varsInScope = varsInScope env <> S.fromList params} body
                <> ")"

          compileBody :: Env -> [Exp] -> String
          compileBody env [exp] = compile env config exp
          compileBody env exps  = "{\n" <> compileBody' env exps <> "}"

          compileBody' :: Env -> [Exp] -> String
          compileBody' env [exp] = case exp of
            (Typed _ _ _ (JSExp _)) -> compile env config exp
            _                         -> "    return " <> compile env config exp <> ";\n"
          compileBody' e (exp : es) = case exp of
            Core.Typed _ _ _ (Core.Assignment name _) ->
              let nextEnv = e { varsInScope = S.insert name (varsInScope e) }
              in  "    " <> compile e config exp <> ";\n" <> compileBody' nextEnv es

            _ -> "    " <> compile e config exp <> ";\n" <> compileBody' e es

        Var ('.' : name) -> "(__R__ => __R__." <> name <> ")"


        Var name ->
          let safeName = generateSafeName name
          in  if safeName == "!" || not coverage then safeName else hpWrapLine coverage astPath l safeName

        Placeholder (ClassRef cls _ call var, ts) exp' -> insertPlaceholderArgs "" e

         where
          insertPlaceholderArgs :: String -> Exp -> String
          insertPlaceholderArgs prev exp'' = case exp'' of
            Core.Typed _ _ _ (Placeholder (ClassRef cls ps call var, ts) exp''') ->
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


        Placeholder (MethodRef cls method var, ts) (Core.Typed _ _ _ (Var name)) ->
          let compiled = generateRecordName optimized cls ts var <> "." <> method <> "()"
          in  if not coverage then compiled else hpWrapLine coverage astPath l compiled

        Assignment name exp ->
          let safeName                       = generateSafeName name
              (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
              content'                       = if coverage && isFunctionType expType
                then "__hpFnWrap('" <> astPath <> "', " <> show l <> ", '" <> safeName <> "')(" <> content <> ")"
                else content
              needsModifier = notElem safeName $ varsInScope env
          in  if not (null dicts)
                then
                  (if needsModifier then "let " else "")
                  <> safeName
                  <> " = "
                  <> placeholders
                  <> content'

                  -- NB: this commented solution enables memoizing for overloaded functions
                  -- but also it does weird things wrt concurrency and Wish, which led tests
                  -- to use the wrong instance of Inspect to display diffs.
                  -- (if needsModifier then "let " else "")
                  -- <> safeName
                  -- <> " = "
                  -- <> placeholders
                  -- <> "{"
                  -- <> "\n  "
                  -- <> intercalate
                  --      "\n  "
                  --      ((\dict -> getGlobalForTarget (cctarget config) <> "." <> dict <> " = " <> dict) <$> dicts)
                  -- <> "\n\n"
                  -- <> "  return "
                  -- <> safeName
                  -- <> "__ND__()"
                  -- <> "\n};\n"
                  -- <> "let "
                  -- <> safeName
                  -- <> "__ND__"
                  -- <> " = "
                  -- <> onceFnName optimized
                  -- <> "(() => "
                  -- <> content'
                  -- <> ")"
                else (if needsModifier then "let " else "") <> safeName <> " = " <> content'

        Export     ass    -> "export " <> compile env config ass

        NameExport name   -> "export { " <> generateSafeName name <> " }"

        Record     fields -> let fs = intercalate "," $ compileField <$> fields in "({" <> fs <> " })"
         where
          compileField :: Field -> String
          compileField (Typed _ _ _ field) = case field of
            Field (name, exp) ->
              " " <> name <> ": " <> hpWrapLine coverage astPath (getStartLine exp) (compile env config exp)
            FieldSpread exp -> " ..." <> hpWrapLine coverage astPath (getStartLine exp) (compile env config exp)

        Access record (Typed _ _ _ (Var name)) ->
          hpWrapLine coverage astPath (getStartLine record) $ compile env config record <> name

        JSExp           content -> content

        ListConstructor elems   -> "([" <> intercalate ", " (compileListItem <$> elems) <> "])"
         where
          compileListItem :: ListItem -> String
          compileListItem (Typed _ _ _ li) = case li of
            ListItem   exp -> compile env config exp
            ListSpread exp -> " ..." <> compile env config exp

        TupleConstructor elems -> "([" <> intercalate ", " (compile env config <$> elems) <> "])"

        Do exps ->
          let compiledExps = compile env config <$> exps
              allExceptLast = init compiledExps
              l = last compiledExps
          in "(() => {\n  "
              <> intercalate "\n  " allExceptLast
              <> "\n  return " <> l
              <> "\n})()"

        Where exp (first : cs) ->
          "((__x__) => {\n  "
            <> compileIs first
            <> concat (("  else " ++) . compileIs <$> cs)
            <> "  else {\n    console.log('non exhaustive patterns for value: ', __x__.toString()); \n    console.trace(); \n    throw 'non exhaustive patterns!';\n  }\n"
            <> "})("
            <> compile env config exp
            <> ")"
         where
          compileListOrTuplePattern :: String -> [Pattern] -> String
          compileListOrTuplePattern scope [] = scope <> ".length === 0"
          compileListOrTuplePattern scope items =
            scope
              <> ".length "
              <> lengthComparator items
              <> " "
              <> show (if containsSpread items then length items - 1 else length items)
              <> " && "
              <> intercalate
                   " && "
                   ((\(item, i) -> compilePattern (scope <> "[" <> show i <> "]") item) <$> zip items [0 ..])
           where
            lengthComparator :: [Pattern] -> String
            lengthComparator pats = if containsSpread pats then ">=" else "==="
            containsSpread :: [Pattern] -> Bool
            containsSpread pats =
              let isSpread = \case
                    Typed _ _ _ (PSpread _) -> True
                    _                       -> False
              in  case find isSpread pats of
                    Just _  -> True
                    Nothing -> False

          compilePattern :: String -> Pattern -> String
          compilePattern scope (Typed _ _ _ pat) = case pat of
            PVar _  -> "true"
            PAny    -> "true"
            PNum  n -> scope <> " === " <> n
            PStr  n -> scope <> " === " <> n
            PBool n -> scope <> " === " <> n
            PCon n [] -> scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\""
            PCon n ps ->
              let args = intercalate " && " $ filter (not . null) $ compileCtorArg scope n <$> zip [0 ..] ps
              in  scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\"" <> if not (null args)
                    then " && " <> args
                    else ""
            PRecord m     -> intercalate " && " $ filter (not . null) $ M.elems $ M.mapWithKey (compileRecord scope) m

            PSpread pat   -> compilePattern scope pat
            PList   items -> compileListOrTuplePattern scope items
            PTuple  items -> compileListOrTuplePattern scope items


          compileIs :: Is -> String
          compileIs (Typed _ (Area (Loc _ l _) _) _ (Is pat exp)) =
            "if ("
              <> (if coverage then "__hp('" <> astPath <> "', 'line', " <> show l <> ", " <> show l <> ") || " else "")
              <> compilePattern "__x__" pat
              <> ") {\n"
              <> buildVars "__x__" pat
              <> "    return "
              <> compile env config exp
              <> ";\n  }\n"

          buildVars :: String -> Pattern -> String
          buildVars v (Typed _ _ _ pat) = case pat of
            PRecord fields ->
              "    let { "
                <> intercalate
                     ", "
                     (filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields)
                <> " } = "
                <> v
                <> ";\n"

            PList  items -> buildTupleOrListVars v items
            PTuple items -> buildTupleOrListVars v items

            PCon _ ps   -> concat $ (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "]") p) <$> zip [0 ..] ps
            PVar n       -> "    let " <> generateSafeName n <> " = " <> v <> ";\n"

            _            -> ""

          buildFieldVar :: String -> Pattern -> String
          buildFieldVar name (Typed _ _ _ pat) = case pat of
            PSpread (Typed _ _ _ (PVar n)) -> "..." <> generateSafeName n
            PVar n -> if null name then generateSafeName n else name <> ": " <> generateSafeName n
            PRecord fields ->
              name
                <> ": { "
                <> intercalate
                     ", "
                     (filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields)
                <> " }"
            PCon _ args -> if null name
              then "{ __args: [" <> intercalate ", " (buildFieldVar "" <$> args) <> "] }"
              else
                name
                <> ": "
                <> "{ __args: ["
                <> intercalate ", " ((\(i, arg) -> buildFieldVar "" arg) <$> zip [0 ..] args)
                <> "] }"
            PList  pats -> name <> ": [" <> intercalate ", " (buildListVar <$> pats) <> "]"
            PTuple pats -> name <> ": [" <> intercalate ", " (buildListVar <$> pats) <> "]"
            _           -> ""

          buildTupleOrListVars :: String -> [Pattern] -> String
          buildTupleOrListVars scope items =
            let itemsStr = buildListVar <$> items
            in  "    let [" <> intercalate "," itemsStr <> "] = " <> scope <> ";\n"

          buildListVar :: Pattern -> String
          buildListVar (Typed _ _ _ pat) = case pat of
            PSpread (Typed _ _ _ (PVar n)) -> "..." <> generateSafeName n
            PVar    n      -> generateSafeName n
            PCon _ args   -> let built = intercalate ", " $ buildListVar <$> args in "{ __args: [" <> built <> "]}"
            PList   pats   -> "[" <> intercalate ", " (buildListVar <$> pats) <> "]"
            PTuple  pats   -> "[" <> intercalate ", " (buildListVar <$> pats) <> "]"
            PRecord fields -> "{ " <> intercalate ", " (M.elems $ M.mapWithKey buildFieldVar fields) <> " }"
            _              -> ""

          compileRecord :: String -> Name -> Pattern -> String
          compileRecord scope n p = compilePattern (scope <> "." <> n) p

          compileCtorArg :: String -> String -> (Int, Pattern) -> String
          compileCtorArg scope _ (x, p) = compilePattern (scope <> ".__args[" <> show x <> "]") p

        Extern (ps :=> t) name foreignName ->
          let paramTypes = getParamTypes t
              paramCount = length ps + length paramTypes
              params = take paramCount ((:"")<$> ['a' ..])
              compiledParams = concat $ (<> " => ") <$> params
              body = foreignName <> concat ((\arg -> "(" <> arg <> ")") <$> params)
          in  "let " <> name <> " = " <> compiledParams <> body

        _ -> "/* Not implemented: " <> ppShow exp <> "*/\n"


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable TypeDecl where
  compile _ _ (Untyped _ _ Alias{}                     ) = ""
  compile _ _ (Untyped _ _ ADT { adtconstructors = [] }) = ""
  compile env config (Untyped _ _ adt) =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>) "" (addExport exported . compile env config <$> ctors)
   where
    addExport :: Bool -> String -> String
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  compile _ config (Untyped _ _ (Constructor cname cparams _)) =
    let coverage  = cccoverage config
        optimized = ccoptimize config
    in  case cparams of
          [] -> "let " <> cname <> " = " <> compileBody cname cparams <> ";\n"
          _ ->
            "let " <> cname <> " = " <> "(" <> compileParams cparams <> " => " <> compileBody cname cparams <> ");\n"
   where
    compileParams n = let argNames = (: []) <$> take (length n) ['a' ..] in intercalate " => " argNames

    compileBody n a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          args     = argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"


compileImport :: CompilationConfig -> Import -> String
compileImport config (Untyped _ _ imp) = case imp of
  NamedImport names _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import { " <> compileNames (generateSafeName . Core.getValue <$> names) <> " } from \"" <> importPath <> "\""
    where compileNames names = if null names then "" else (init . init . concat) $ (++ ", ") <$> names
  DefaultImport alias _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import " <> Core.getValue alias <> " from \"" <> importPath <> "\""


buildImportPath :: CompilationConfig -> FilePath -> FilePath
buildImportPath config absPath =
  let outputPath = ccoutputPath config
      rootPath   = ccrootPath config
      astPath    = ccastPath config
      destPath   = dropFileName $ computeTargetPath outputPath rootPath astPath
      depPath    = computeTargetPath outputPath rootPath absPath
  in  convertWindowsSeparators $ cleanRelativePath $ replaceExtension (joinPath ["./", makeRelativeEx destPath depPath]) ".mjs"


updateASTPath :: FilePath -> CompilationConfig -> CompilationConfig
updateASTPath astPath config = config { ccastPath = astPath }

instance Compilable Core.Interface where
  compile _ config (Untyped _ _ interface) = case interface of
    Core.Interface name _ _ _ _ -> getGlobalForTarget (cctarget config) <> "." <> name <> " = {};\n"

instance Compilable Core.Instance where
  compile env config (Untyped _ _ inst) = case inst of
    Core.Instance "Eq" _ _ _ -> ""

    Core.Instance interface _ typings dict -> interface <> "['" <> typings <> "'] = {};\n" <> concat
      (uncurry compileMethod <$> M.toList (M.map fst dict))
     where
      compileMethod :: Name -> Exp -> String
      compileMethod n (Core.Typed (_ :=> t) (Area (Loc _ line _) _) _ (Core.Assignment _ exp)) =
        let
          (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
          content'                       = if cccoverage config && isFunctionType t
            then "__hpFnWrap('" <> ccastPath config <> "', " <> show line <> ", '" <> n <> "')(" <> content <> ")"
            else content
          instRoot = interface <> "['" <> typings <> "']"
          compiledMethod =
            instRoot
              <> "['"
              <> n
              <> "'] = () => "
              <> placeholders
              <> content'
              <> "\n"
        in
          if not (null dicts)
            then compiledMethod
            else instRoot <> "['" <> n <> "'] = () => " <> content' <> ";\n"
      -- compileMethod :: Name -> Exp -> String
      -- compileMethod n (Core.Typed t (Area (Loc _ line _) _) (Core.Assignment _ exp)) =
      --   let
      --     (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
      --     content'                       = if cccoverage config && isFunctionType t
      --       then "__hpFnWrap('" <> ccastPath config <> "', " <> show line <> ", '" <> n <> "')(" <> content <> ")"
      --       else content
      --     instRoot = interface <> "['" <> typings <> "']"
      --     compiledNDMethod =
      --       "let __"
      --         <> interface
      --         <> typings
      --         <> n
      --         <> " = "
      --         <> onceFnName (ccoptimize config)
      --         <> "(() => "
      --         <> content'
      --         <> ");\n"
      --     compiledMethod =
      --       instRoot
      --         <> "['"
      --         <> n
      --         <> "'] = () => "
      --         <> placeholders
      --         <> "{\n  "
      --         <> intercalate
      --              "\n  "
      --              ((\dict -> getGlobalForTarget (cctarget config) <> "." <> dict <> " = " <> dict) <$> dicts)
      --         <> "\n  return __"
      --         <> interface
      --         <> typings
      --         <> n
      --         <> "();\n};\n"
      --   in
      --     if not (null dicts)
      --       then compiledNDMethod <> compiledMethod
      --       else instRoot <> "['" <> n <> "'] = () => " <> content' <> ";\n"


compileAssignmentWithPlaceholder :: Env -> CompilationConfig -> Exp -> (String, [String], String)
compileAssignmentWithPlaceholder env config fullExp@(Core.Typed _ _ _ exp) = case exp of
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
  Core.Typed _ _ _ (Core.Assignment name _) ->
    let nextEnv = env { varsInScope = S.insert name (varsInScope env) }
    in  compile env config exp <> ";\n" <> compileExps nextEnv config es

  _ -> compile env config exp <> ";\n" <> compileExps env config es


buildDefaultExport :: [TypeDecl] -> [Exp] -> String
buildDefaultExport as es =
  let expExportNames    = generateSafeName . getExportName <$> filter isExport es
      adtExportNames    = getConstructorName <$> concat (adtconstructors . getValue <$> filter isADTExport as)
      allDefaultExports = expExportNames <> adtExportNames
  in  case allDefaultExports of
        []      -> "export default {};\n"
        exports -> "export default { " <> intercalate ", " exports <> " };\n"

 where
  isADTExport :: TypeDecl -> Bool
  isADTExport (Untyped _ _ ADT { adtexported }) = adtexported
  isADTExport _                               = False

  isExport :: Exp -> Bool
  isExport a = case a of
    Typed _ _ _ (Export _) -> True

    _ -> False

  getExportName :: Exp -> String
  getExportName (Typed _ _ _ (Export (Typed _ _ _ (Assignment n _)))) = n
  getExportName (Typed _ _ _ (Export (Typed _ _ _ (Extern _ n _))))   = n
  

  getConstructorName :: Constructor -> String
  getConstructorName (Untyped _ _ (Constructor cname _ _)) = cname
