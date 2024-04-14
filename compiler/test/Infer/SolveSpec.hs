module Infer.SolveSpec where

import qualified Data.Map                      as M
import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                )
import           Test.Hspec.Golden              ( Golden(..) )
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )

import qualified AST.Source                    as Src
import qualified AST.Solved                    as Slv
import           Canonicalize.CanonicalM
import           Error.Error
import           Run.Target
import           Run.OptimizationLevel
import           Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import           Error.Warning
import qualified Driver
import           Run.Options
import           Utils.PathUtils
import           Driver (Prune(Don'tPrune))
import qualified Data.List                     as List
import qualified System.Directory              as Dir
import qualified Rock
import qualified Driver.Query as Query
import           GHC.IO (unsafePerformIO)
import           Control.Monad (forM)
import           System.FilePath (normalise)
import Explain.Location


snapshotTest :: Show a => String -> a -> Golden Text
snapshotTest name actualOutput =
  Golden
    { output        = pack $ ppShow actualOutput
    , encodePretty  = ppShow
    , writeToFile   = T.writeFile
    , readFromFile  = T.readFile
    , goldenFile    = ".snapshots/" <> unpack (replace (pack " ") (pack "_") (pack name)) <> "/golden"
    , actualFile    = Just $ ".snapshots/" <> unpack (replace (pack " ") (pack "_") (pack name)) <> "/actual"
    , failFirstTime = False
    }


buildOptions :: FilePath -> PathUtils -> Options
buildOptions entrypoint pathUtils =
  Options
    { optPathUtils = pathUtils { canonicalizePath = return, normalisePath = \p -> if "__BUILTINS__.mad" `List.isSuffixOf` p then normalise p else (("./"++) . normalise) p }
    , optEntrypoint = entrypoint
    , optRootPath = "./"
    , optOutputPath = "./build"
    , optTarget = TNode
    , optOptimized = False
    , optDebug = False
    , optBundle = False
    , optCoverage = False
    , optGenerateDerivedInstances = True
    , optInsertInstancePlaholders = True
    , optParseOnly = False
    , optMustHaveMain = True
    , optOptimizationLevel = O3
    }


renameBuiltinsImport :: Slv.Import -> Slv.Import
renameBuiltinsImport imp = case imp of
  Slv.Untyped area (Slv.DefaultImport (Slv.Untyped (Area (Loc 0 0 0) (Loc 0 0 0)) "__BUILTINS__") "__BUILTINS__" _) ->
    Slv.Untyped area (Slv.DefaultImport (Slv.Untyped (Area (Loc 0 0 0) (Loc 0 0 0)) "__BUILTINS__") "__BUILTINS__" "__BUILTINS__")

  or ->
    or


inferModule :: String -> IO (Slv.AST, [CompilationWarning], [CompilationError])
inferModule code = do
  let modulePath = "Module.mad"
  let options = buildOptions modulePath defaultPathUtils
  initialState <- Driver.initialState
  ((ast, _), warnings, errors) <- Driver.runIncrementalTask
    initialState
    options
    [modulePath]
    (M.singleton modulePath code)
    Don'tPrune
    (Rock.fetch $ Query.SolvedASTWithEnv modulePath)

  return (ast { Slv.aimports = map renameBuiltinsImport (Slv.aimports ast) }, warnings, errors)

inferModuleWithoutMain :: String -> IO (Slv.AST, [CompilationWarning], [CompilationError])
inferModuleWithoutMain code = do
  let modulePath = "Module.mad"
  let options = (buildOptions modulePath defaultPathUtils { doesFileExist = \p -> if "__BUILTINS__.mad" `List.isSuffixOf` p then Dir.doesFileExist p else return True }) { optMustHaveMain = False }
  initialState <- Driver.initialState
  ((ast, _), warnings, errors) <- Driver.runIncrementalTask
    initialState
    options
    [modulePath]
    (M.singleton modulePath code)
    Don'tPrune
    (Rock.fetch $ Query.SolvedASTWithEnv modulePath)

  return (ast { Slv.aimports = map renameBuiltinsImport (Slv.aimports ast) }, warnings, errors)


inferManyModules :: FilePath -> M.Map FilePath String -> IO (M.Map FilePath Slv.AST, [CompilationWarning], [CompilationError])
inferManyModules entrypoint modules = do
  let options =
        buildOptions
          entrypoint
          defaultPathUtils
            { doesFileExist = \p -> if "__BUILTINS__.mad" `List.isSuffixOf` p then Dir.doesFileExist p else return True }
  initialState <- Driver.initialState
  let task = do
        solved <- forM (M.keys modules) $ \path -> do
          (solvedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
          return (path, solvedAst)
        return $ M.fromList solved

  (solvedModules, warnings, errors) <- Driver.runIncrementalTask
    initialState
    options
    [entrypoint]
    modules
    Don'tPrune
    task
  -- return (solvedModules, warnings, errors)
  return (M.map (\ast -> ast { Slv.aimports = map renameBuiltinsImport (Slv.aimports ast) }) solvedModules, warnings, errors)

inferManyModulesWithoutMain :: FilePath -> M.Map FilePath String -> IO (M.Map FilePath Slv.AST, [CompilationWarning], [CompilationError])
inferManyModulesWithoutMain entrypoint modules = do
  let options = (buildOptions entrypoint defaultPathUtils { doesFileExist = \p -> if "__BUILTINS__.mad" `List.isSuffixOf` p then Dir.doesFileExist p else return True }) { optMustHaveMain = False }
  initialState <- Driver.initialState
  let task = do
        solved <- forM (M.keys modules) $ \path -> do
          (solvedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
          return (path, solvedAst)
        return $ M.fromList solved

  (solvedModules, warnings, errors) <- Driver.runIncrementalTask
    initialState
    options
    [entrypoint]
    modules
    Don'tPrune
    task
  return (M.map (\ast -> ast { Slv.aimports = map renameBuiltinsImport (Slv.aimports ast) }) solvedModules, warnings, errors)


spec :: Spec
spec = do
  describe "infer" $ do
    it "should infer abstractions" $ do
      let code   = "add = (b, c) => b + c"
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer abstractions" actual

    it "should infer assignments" $ do
      let code   = unlines ["fn = (b, c) => (b + c)", "main = () => { fn(2, 3) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer assignments" actual

    it "should infer minus operator" $ do
      let code   = "substract = (b, c) => (b - c)"
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer minus operator" actual

    it "should infer multiplication operator" $ do
      let code   = "multiply = (b, c) => (b * c)"
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer multiplication operator" actual

    it "should infer division operator" $ do
      let code   = "divide = (b, c) => (b / c)"
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer division operator" actual

    it "should infer equality operator" $ do
      let code   = "main = () => { 1 == 3 }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer equality operator" actual

    it "should infer wrapped douleEq operator" $ do
      let code   = "x = ((a, b) => (a == b))(1, 3)"
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer wrapped douleEq operator" actual

    it "should infer an empty source" $ do
      let code   = ""
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer an empty source" actual

    it "should fail for unbound variables" $ do
      let code   = "main = () => { x }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail for unbound variables" actual

    ---------------------------------------------------------------------------


    -- ADTs:

    it "should infer adts" $ do
      let code   = unlines
                    [ "type Result = Success(String) | Error"
                    , "main = () => {"
                    , "  result = Success(\"response\")"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer adts" actual

    it "should infer adts with type parameters" $ do
      let code   = unlines
                    [ "type Result a"
                    , "  = Success(a)"
                    , "  | Error"
                    , "main = () => {"
                    , "  result = Success(true)"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer adts with type parameters" actual

    it "should infer application of adts" $ do
      let code = unlines
            [ "type Result = Success(String) | Error"
            , "main = () => {"
            , "  result1 = Success(\"response\")"
            , "  result2 = Error"
            , "  ((a, b) => (a == b))(result1, result2)"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer application of adts" actual

    it "should infer adt return for abstractions" $ do
      let code = unlines
            [ "type Result a = Success(a) | Error"
            , "main = () => {"
            , "  result1 = Success(\"response\")"
            , "  result2 = Error"
            , "  ((a, b) => (a == b))(result1, result2)"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer adt return for abstractions" actual

    it "should return an error when an ADT defines a type already existing" $ do
      let code   = unlines ["type Result a = Success a | Error", "type Result a = Success a | Error"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should return an error when an ADT defines a type already existing" actual

    it "should fail to infer ADTs with constructors that have free vars" $ do
      let code   = unlines ["type Result = Success(b)"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should fail to infer ADTs with constructors that have free vars" actual

    it "should infer adts with record constructors" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "type Result = Success({ value :: String }) | Error({ message :: String })"
            , "main = () => {"
            , "  result1 = Success({ value: `42` })"
            , "  result2 = Error({ message: \"Err\" })"
            , "  ((a, b) => (a == b))(result1, result2)"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer adts with record constructors" actual

    it "should infer params for adts" $ do
      let code = unlines
                  [ "instance Semigroup String {"
                  , "  assoc = #--#"
                  , "}"
                  , ""
                  , "instance Monoid String {"
                  , "  mempty = \"\""
                  , "  mappend = assoc"
                  , "  mconcat = #--#"
                  , "}"
                  , ""
                  , "type Result = Success({ value :: String })"
                  , "main = () => {"
                  , "  r = Success({ value: \"42\" })"
                  , "  ((a) => a)(r)"
                  , "}"
                  ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer params for adts" actual

    it "should fail if it uses an ADT not defined" $ do
      let code =
            unlines
              [ "addNegativeTen :: Maybe Integer -> Integer"
              , "export addNegativeTen = (a) => (a + -10)"
              , "main = () => {"
              , "  addNegativeTen(3)"
              , "}"
              ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail if it uses an ADT not defined" actual

    it "should fail if it uses an ADT not defined in patterns" $ do
      let code   = unlines
                    [ "main = () => {"
                    , "  where(3) {"
                    , "    NotExisting => 5"
                    , "  }"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail if it uses an ADT not defined in patterns" actual

    it "should resolve ADTs with function parameters" $ do
      let code   = unlines
                    [ "export type Wish e a = Wish ((e -> {}) -> (a -> {}) -> {})"
                    , "main = () => {"
                    , "  Wish((bad, good) => (good(3)))"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve ADTs with function parameters" actual

    it "should resolve namespaced ADTs in patterns" $ do
      let codeA = "export type Maybe a = Just(a) | Nothing"
          codeB = unlines
            [ "import M from \"./ModuleA\""
            , "fn :: M.Maybe Integer -> Integer"
            , "export fn = (x) => ("
            , "  where(x) {"
            , "    M.Just(a) => a"
            , "    M.Nothing => -3"
            , "  }"
            , ")"
            ]
          actual = unsafePerformIO $ inferManyModulesWithoutMain "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should resolve namespaced ADTs in patterns" actual

    it "should resolve namespaced ADTs in other ADTs" $ do
      let
        codeA = "export type Maybe a = Just(a) | Nothing"
        codeB = unlines
            ["import M from \"./ModuleA\""
            , "type MyType = MyType(M.Maybe String)"
            , "main = () => {"
            , "  x = MyType(M.Just(\"3\"))"
            , "}"
            ]
        actual = unsafePerformIO $ inferManyModules "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should resolve namespaced ADTs in other ADTs" actual

    it "should allow ADT constructors to have record parameters" $ do
      let
        codeA = "export type Point = Point(#[Integer, Integer])"
        codeB = unlines
          [ "import P from \"./ModuleA\""
          , "main = () => {"
          , "  p = P.Point(#[2, 4])"
          , "  where(p) {"
          , "    P.Point(#[a, b]) => a + b"
          , "  }"
          , "}"
          ]
        actual = unsafePerformIO $ inferManyModules "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should allow ADT constructors to have record parameters" actual

    -------------------------------------------------------------------



    -- Type Aliases:

    it "should allow aliasing of types" $ do
      let codeA = unlines
            [ "interface Functor m {"
            , "  map :: (a -> b) -> m a -> m b"
            , "}"
            , ""
            , "interface Functor m => Applicative m {"
            , "  ap :: m (a -> b) -> m a -> m b"
            , "  pure :: a -> m a"
            , "}"
            , ""
            , "interface Applicative m => Monad m {"
            , "  of :: a -> m a"
            , "  chain :: (a -> m b) -> m a -> m b"
            , "}"
            , ""
            , "export type Wish e a = Wish((e -> {}) -> (a -> {}) -> {})"
            , ""
            , ""
            , "instance Functor (Wish e) {"
            , "  map = (f, m) => Wish((badCB, goodCB) =>"
            , "    where(m) {"
            , "      Wish(run) => run(badCB, (x) => (goodCB(f(x))))"
            , "    }"
            , "  )"
            , "}"
            , ""
            , "instance Applicative (Wish e) {"
            , "  pure = (a) => Wish((badCB, goodCB) => goodCB(a))"
            , ""
            , "  ap = (mf, m) => Wish((badCB, goodCB) => where(#[mf, m]) {"
            , "    #[Wish(runMF), Wish(runM)] =>"
            , "      runM("
            , "        badCB,"
            , "        (x) => runMF("
            , "          badCB,"
            , "          (f) => goodCB(f(x))"
            , "        )"
            , "      )"
            , "  })"
            , "}"
            , ""
            , "instance Monad (Wish e) {"
            , "  of = pure"
            , ""
            , "  chain = (f, m) => Wish((badCB, goodCB) =>"
            , "    where(m) {"
            , "      Wish(run) =>"
            , "        run(badCB, (x) =>"
            , "          where(f(x)) {"
            , "            Wish(r) => r(badCB, goodCB)"
            , "          }"
            , "        )"
            , "    }"
            , "  )"
            , "}"
            , ""
            , ""
            , "mapRej :: (e -> f) -> Wish e a -> Wish f a"
            , "export mapRej = (f, m) => ("
            , "  Wish((badCB, goodCB) => ("
            , "    where(m) {"
            , "      Wish(run) => run((x) => (badCB(f(x))), goodCB)"
            , "    }"
            , "  ))"
            , ")"
            , ""
            , ""
            , "chainRej :: (e -> Wish f a) -> Wish e a -> Wish f a"
            , "export chainRej = (f, m) => ("
            , "  Wish((badCB, goodCB) => ("
            , "    where(m) {"
            , "      Wish(run) => run((x) => ("
            , "        where(f(x)) {"
            , "          Wish(r) => r(badCB, goodCB)"
            , "        }"
            , "      ), goodCB)"
            , "    }"
            , "  ))"
            , ")"
            , ""
            , ""
            , "good :: a -> Wish e a"
            , "export good = (a) => Wish((badCB, goodCB) => goodCB(a))"
            , ""
            , "bad :: e -> Wish e a"
            , "export bad = (e) => ("
            , "  Wish((badCB, goodCB) => badCB(e))"
            , ")"
            , ""
            , ""
            , "getWishFn = (w) => (where(w) {"
            , "  Wish(fn) => fn"
            , "})"
            , ""
            , ""
            , "parallel :: List (Wish e a) -> Wish e (List a)"
            , "export parallel = (wishes) => ("
            , "  Wish((badCB, goodCB) => (#- {"
            , "    const l = wishes.length"
            , "    let ko = false;"
            , "    let ok = 0;"
            , "    const out = new Array(l);"
            , "    const next = j => (j === l && goodCB(out));"
            , "    const fork = (w, j) => (getWishFn(w)("
            , "      e => ko || (badCB(e), ko = true),"
            , "      x => ko || (out[j] = x, next(++ok))"
            , "    ));"
            , "    wishes.forEach(fork);"
            , "  } -#))"
            , ")"
            , ""
            , ""
            , "fulfill :: (e -> {}) -> (a -> {}) -> Wish e a -> {}"
            , "export fulfill = (badCB, goodCB, m) => {"
            , "  where(m) {"
            , "    Wish(run) => run(badCB, goodCB)"
            , "  }"
            , ""
            , "  return {}"
            , "}"
            ]
          codeB = unlines
            [ "import W from \"./ModuleA\""
            , ""
            , "double :: Functor f => f Integer -> f Integer"
            , "double = map((x) => x * 2)"
            , ""
            , "main = () => {"
            , "  of(3)"
            , "    |> map((x) => (x + 3))"
            , "    |> chain((x) => (of(x * 3)))"
            , "    |> W.fulfill((a) => ({}), (a) => ({}))"
            , "}"
            ]
          actual = unsafePerformIO $ inferManyModules "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should allow aliasing of types" actual

    ---------------------------------------------------------------------------



    -- Interfaces:

    it "should fail when the type is ambiguous" $ do
      let code = unlines
            [ "interface Read a {"
            , "  read :: String -> a"
            , "}"
            , ""
            , "instance Read Integer {"
            , "  read = (s) => (#- parseFloat(s, 10) -#)"
            , "}"
            , ""
            , "main = () => {"
            , "  read(\"3\")"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when the type is ambiguous" actual

    it "should resolve ambiguity with type annotations" $ do
      let code = unlines
            [ "interface Read a {"
            , "  read :: String -> a"
            , "}"
            , ""
            , "instance Read Integer {"
            , "  read = (s) => (#- parseFloat(s, 10) -#)"
            , "}"
            , ""
            , "main = () => {"
            , "  (read(\"3\") :: Integer)"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve ambiguity with type annotations" actual

    it "should resolve constrained instances" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "interface Inspect a {"
            , "  inspect :: a -> String"
            , "}"
            , ""
            , "instance Inspect Boolean {"
            , "  inspect = (b) => b ? \"True\" : \"False\""
            , "}"
            , ""
            , "instance Inspect Integer {"
            , "  inspect = (n) => (#- new Integer(n).toString() -#)"
            , "}"
            , ""
            , "instance (Inspect a, Inspect b) => Inspect #[a, b] {"
            , "  inspect = where { #[a, b] => \"#[\" ++ inspect(a) ++ \", \" ++ inspect(b) ++ \"]\" }"
            , "}"
            , ""
            , "main = () => {"
            , "  inspect(#[1, false])"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve constrained instances" actual

    it "should fail for instances missing constraints" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "interface Inspect a {"
            , "  inspect :: a -> String"
            , "}"
            , ""
            , "instance Inspect Boolean {"
            , "  inspect = (b) => b ? \"True\" : \"False\""
            , "}"
            , ""
            , "instance Inspect Integer {"
            , "  inspect = (n) => (#- new Integer(n).toString() -#)"
            , "}"
            , ""
            , "instance Inspect #[a, b] {"
            , "  inspect = where { #[a, b] => \"#[\" ++ inspect(a) ++ \", \" ++ inspect(b) ++ \"]\" }"
            , "}"
            , ""
            , "main = () => {"
            , "  inspect(#[1, false])"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail for instances missing constraints" actual

    it "should fail when no instance is found" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "interface Inspect a {"
            , "  inspect :: a -> String"
            , "}"
            , ""
            , "instance Inspect Boolean {"
            , "  inspect = (b) => b ? \"True\" : \"False\""
            , "}"
            , ""
            , "instance (Inspect a, Inspect b) => Inspect #[a, b] {"
            , "  inspect = where { #[a, b] => \"#[\" ++ inspect(a) ++ \", \" ++ inspect(b) ++ \"]\" }"
            , "}"
            , ""
            , "main = () => {"
            , "  inspect(3)"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when no instance is found" actual



    ---------------------------------------------------------------------------



    -- Records:

    it "should infer a record field access" $ do
      let code   = unlines
                    [ "instance Semigroup String {"
                    , "  assoc = #--#"
                    , "}"
                    , ""
                    , "instance Monoid String {"
                    , "  mempty = \"\""
                    , "  mappend = assoc"
                    , "  mconcat = #--#"
                    , "}"
                    , ""
                    , "main = () => {"
                    , "  a = { x: 3.1415, y: -500 }"
                    , "  a.x"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer a record field access" actual

    it "should infer an App with a record" $ do
      let code   = unlines
                    [ "instance Semigroup String {"
                    , "  assoc = #--#"
                    , "}"
                    , ""
                    , "instance Monoid String {"
                    , "  mempty = \"\""
                    , "  mappend = assoc"
                    , "  mconcat = #--#"
                    , "}"
                    , ""
                    , "main = () => {"
                    , "  a = { x: 3, y: 5 }"
                    , "  xPlusY = (r) => (r.x + r.y)"
                    , "  xPlusY(a)"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer an App with a record" actual

    it "should fail to infer record if their fields do not match" $ do
      let code   = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "main = () => { { x: 3, y: 5 } == { name: \"John\" } }"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail to infer record if their fields do not match" actual

    it "should infer a record with a type annotation" $ do
      let code   = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "main = () => { ({ x: 3, y: 7 } :: { x :: Integer, y :: Integer }) }"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer a record with a type annotation" actual

    it "should infer abstraction param that is a deep record" $ do
      let code   = "f = (x) => (x.a.b.c.d.e)"
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer abstraction param that is a deep record" actual

    it "should infer abstraction param that have a record exp as body" $ do
      let code   = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "addTodo = (state) => ({ ...state, x: \"3\", y: state.y })"
            ]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer abstraction param that have a record exp as body" actual

    it "should fail when spreading a non spreadable type into a record" $ do
      let code   = unlines ["main = () => { { ...3, x: 1 } }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when spreading a non spreadable type into a record" actual

    it "correctly infer various record transformations" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "main = () => {"
            , "  ff = (record) => (["
            , "    record.x,"
            , "    record.z,"
            , "    ...record.y"
            , "  ])"
            , "  "
            , "  "
            , "  fr1 = (x) => ({ ...x, p: 3 })"
            , "  "
            , "  fr2 = (r, x) => ({ ...r, p: x })"
            , "  "
            , "  r0 = fr1({ z: 9, p: 3 })"
            , "  r1 = { ...fr1({ z: 9, p: 3 }), y: 5 }"
            , "  r2 = fr2({ p: '4', g: 5 }, '5')"
            , "  "
            , "  fxy = (s, e) => ({"
            , "    ...e,"
            , "    c: s.x + 1,"
            , "    b: '3'"
            , "  })"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "correctly infer various record transformations" actual

    it "correctly infer recursive calls that aren't in tail position" $ do
      let code = unlines
            [" stuff = () => {"
            , "  run = () => {"
            , "    where(\"\") {"
            , "      \"continue\" =>"
            , "        {}"
            , ""
            , "      _ => do {"
            , "        run()"
            , "      }"
            , "    }"
            , "  }"
            , ""
            , "  run()"
            , "}"
            ]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "correctly infer recursive calls that aren't in tail position" actual

    it "should infer complex where expressions with records" $ do
      let code = unlines
            [ "type Comparison"
            , "  = LT"
            , "  | EQ"
            , "  | GT"
            , ""
            , "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "interface Functor m {"
            , "  map :: (a -> b) -> m a -> m b"
            , "}"
            , "instance Functor List {"
            , "  map = (f, xs) => #- xs.map((x) => f(x)) -#"
            , "}"
            , ""
            , "customCompare :: a -> a -> Comparison"
            , "customCompare = #--#"
            , ""
            , "sortBy :: (a -> a -> Comparison) -> List a -> List a"
            , "export sortBy = (fn, xs) => #- xs.sort((a, b) => fn(a)(b)) -#"
            , ""
            , "chain :: (a -> List b) -> List a -> List b"
            , "chain = #--#"
            , ""
            , "FunctionLink = where { f => \"moduleName\" ++ f.name }"
            , "generateFunctionLinks = pipe("
            , "  chain(.expressions),"
            , "  sortBy((a, b) => where(#[a, b]) {"
            , "    #[{ name: nameA }, { name: nameB }] => customCompare(nameA, nameB)"
            , "    #[{ name: nameC }, { ik: nameD }] => customCompare(nameC, nameD)"
            , "    #[{ tchouk: nameD }, { name: nameC }] => customCompare(nameC, nameD)"
            , "    #[{ name: nameC }, { lui: nameD }] => customCompare(nameC, nameD)"
            , "    #[{ name: nameC }, { po: { pi: { nameD }}}] => customCompare(nameC, nameD)"
            , "    #[{ po: { pi: { nameC }}}, { name: nameD }] => customCompare(nameC, nameD)"
            , "  }),"
            , "  map(FunctionLink)"
            , ")"
            ]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer complex where expressions with records" actual

    it "should infer record params that are partially used in abstractions" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "type Maybe a = Just(a) | Nothing"
            , "find :: (a -> Boolean) -> List a -> Maybe a"
            , "export find = (predicate, xs) => (#- {"
            , "  const found = xs.find(predicate);"
            , "  if (found === undefined) {"
            , "    return Nothing"
            , "  }"
            , "  else {"
            , "    return Just(found)"
            , "  }"
            , "} -#)"
            , "flip :: (a -> b -> c) -> (b -> a -> c)"
            , "export flip = (fn) => ("
            , "  (b, a) => (fn(a, b))"
            , ")"
            , ""
            , "propOrWrappedDefault :: w -> z -> String -> x -> y"
            , "export propOrWrappedDefault = (Wrap, def, px, ob) => ( #- {"
            , "  return ("
            , "    !!ob[px] ?"
            , "    Just(ob[px]) :"
            , "    Wrap(def)"
            , "  )"
            , "} -# )"
            , "export prop = propOrWrappedDefault(Nothing, \"\")"
            , ""
            , "export propOr = propOrWrappedDefault(Just)"
            , "propEq :: e -> String -> o -> Boolean"
            , "export propEq = (xx, px, ob) => (#-{"
            , "  return !!ob[px] && ob[px] === xx"
            , "}-#)"
            , ""
            , "objOf :: String -> x -> y"
            , "export objOf = (px, x) => (#-{ return { [px]: x } }-#)"
            , ""
            , "merge :: a -> b -> c"
            , "export merge = (a, b) => (#- Object.assign({}, a, b) -#) "
            , ""
            , "mergeLeft :: b -> a -> c"
            , "mergeLeft = flip(merge)"
            , ""
            , "alias ShopDiscount = {"
            , "  id :: String,"
            , "  itemId :: Integer,"
            , "  multiplier :: Integer"
            , "}"
            , "alias ShopItem = {"
            , "  id :: Integer,"
            , "  description :: String,"
            , "  cost :: Integer,"
            , "  discounts :: List ShopDiscount"
            , "}"
            , "alias ShopCustomer = {"
            , "  id :: String,"
            , "  cart :: List ShopItem,"
            , "  money :: Integer"
            , "}"
            , "alias ShopContext = {"
            , "  customers :: List ShopCustomer,"
            , "  inventory :: List ShopItem,"
            , "  sales :: List ShopDiscount"
            , "}"
            ]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should infer record params that are partially used in abstractions" actual

    it "should infer extensible records with typed holes" $ do
      let code   = unlines
                    [ "instance Semigroup String {"
                    , "  assoc = #--#"
                    , "}"
                    , ""
                    , "instance Monoid String {"
                    , "  mempty = \"\""
                    , "  mappend = assoc"
                    , "  mconcat = #--#"
                    , "}"
                    , ""
                    , "type DateTime = DateTime"
                    , ""
                    , "now :: a -> DateTime"
                    , "now = #--#"
                    , ""
                    , "toISOString :: DateTime -> String"
                    , "toISOString = #--#"
                    , ""
                    , "log :: String -> {}"
                    , "log = #--#"
                    , ""
                    , "addFourthDimension :: { ...a, } -> { ...a, time :: DateTime, }"
                    , "addFourthDimension = (input) => ({ ...input, time: now() })"
                    , ""
                    , "main = () => {"
                    , "  pipe("
                    , "    ???,"
                    , "    addFourthDimension,"
                    , "    ???,"
                    , "    (fourD) => ({ ...fourD, time: toISOString(fourD.time) }),"
                    , "    ???,"
                    , "    log"
                    , "  )({ x: 11, y: 7, z: 9 })"
                    , "}"
                    ]

          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer extensible records with typed holes" actual

    ---------------------------------------------------------------------------


    -- Lists:

    it "should infer list constructors" $ do
      let code   = unlines
                    [ "main = () => {"
                    , "  []"
                    , "  [1, 2, 3]"
                    , "  [\"one\", \"two\", \"three\"]"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer list constructors" actual

    it "should infer list spread" $ do
      let code   = unlines ["main = () => { [ 1, ...[1, 2]] }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer list spread" actual

    it "should fail when spreading an list of a different type" $ do
      let code   = unlines ["main = () => { [ 1, ...[\"1\", \"2\"]] }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when spreading an list of a different type" actual

    it "should fail when spreading a non spreadable type into an list" $ do
      let code   = unlines ["main = () => { [1, ...3] }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when spreading a non spreadable type into an list" actual

    it "should fail when constructing a list with different types" $ do
      let code   = unlines ["main = () => { [1, false, 3, 4] }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when constructing a list with different types" actual

    -- Tuples

    it "should infer tuple constructors" $ do
      let code   = unlines
                    [ "main = () => {"
                    , "  #[1, 2, 3]"
                    , "  #[true, \"John\", 33]"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer tuple constructors" actual


    ---------------------------------------------------------------------------


    -- Applications:

    it "should fail for applications with a wrong argument type" $ do
      let code   = unlines ["fn = (a, b) => (a == b)", "main = () => { fn(\"3\", 4) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail for applications with a wrong argument type" actual

    it "should infer applications where the abstraction results from an application" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "type Maybe a = Just(a) | Nothing"
            , "flip :: (a -> b -> c) -> (b -> a -> c)"
            , "export flip = (fn) => ("
            , "  (b, a) => fn(a, b)"
            , ")"
            , ""
            , "nth :: Integer -> List a -> Maybe a"
            , "export nth = (i, xs) => (#- {"
            , "  const x = xs[i];"
            , "  return x === undefined"
            , "    ? Nothing()"
            , "    : Just(x);"
            , "} -#)"
            , ""
            , "names = ["
            , "  \"alice\", \"bob\", \"caroline\", \"david\", \"elizabeth\", \"frances\", \"georgina\", \"harold\""
            , "]"
            , "log = (a) => (#- { console.log(a); return a; } -#)"
            , "mash :: String -> String -> String"
            , "mash = (a, b) => (a ++ b)"
            , ""
            , "main = () => {"
            , "  mash(\">>\", \"shit\") |> log"
            , "  nth(2, names) |> log"
            , "  flip(mash)(\">>\", \"shit\") |> log"
            , "  thn = flip(nth)"
            , "  thn(names, 2) |> log"
            , "  flip(nth)(names, 2) |> log"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer applications where the abstraction results from an application" actual

    it "should fail to infer applications when a variable is used with different types" $ do
      let code = unlines
            [ "instance Semigroup String {"
            , "  assoc = #--#"
            , "}"
            , ""
            , "instance Monoid String {"
            , "  mempty = \"\""
            , "  mappend = assoc"
            , "  mconcat = #--#"
            , "}"
            , ""
            , "type Maybe a = Just(a) | Nothing"
            , ""
            , "fromMaybe :: a -> Maybe a -> a"
            , "fromMaybe = (a, maybe) => #- -#"
            , ""
            , "export type Wish e a = Wish((e -> {}) -> (a -> {}) -> {})"
            , ""
            , "of = (a) => Wish((bad, good) => good(a))"
            , ""
            , "nth :: Integer -> List a -> Maybe a"
            , "nth = (i, xs) => #- -#"
            , ""
            , "slice :: Integer -> Integer -> List a -> List a"
            , "export slice = (start, end, xs) => #- xs.slice(start, end) -#"
            , ""
            , "len :: List a -> Integer"
            , "export len = (xs) => #- xs.length -#"
            , ""
            , "export alias Action a = a -> String -> List (Wish (a -> a) (a -> a))"
            , ""
            , ""
            , "type Todo = Todo(String, Boolean)"
            , "alias State = { input :: String, todos :: List Todo }"
            , ""
            , "res = pipe("
            , "  (x) => x + 1,"
            , "  (x) => x - 1"
            , ")(3)"
            , ""
            , "toggleTodo :: Integer -> Action State"
            , "toggleTodo = (index, _, __) => ["
            , "  of((state) => pipe("
            , "    .todos,"
            , "    nth(index),"
            , "    fromMaybe(Todo(\"Oups\", false)),"
            , "    where { Todo(txt, checked) => Todo(txt, !checked) },"
            , "    (toggled) => ({"
            , "      ...state,"
            , "      todos: slice(0, len(state.todos), state)"
            , "    })"
            , "  )(state))"
            , "]"
            ]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should fail to infer applications when a variable is used with different types" actual



    ---------------------------------------------------------------------------


    -- Abstractions:

--     -- TODO: Write and implement the same test with ADTs
--     -- TODO: Write tests where implementation and definition don't match to force
--     -- implementing it as it's currently not implemented.
    it "should resolve abstractions with a type definition" $ do
      let code   = unlines ["fn :: Integer -> Integer -> Boolean", "fn = (a, b) => (a == b)", "main = () => { fn(3, 4) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve abstractions with a type definition" actual

    it "should fail for abstractions with a wrong type definition" $ do
      let code   = unlines ["fn :: String -> Integer -> Boolean", "fn = (a, b) => (a == b)", "main = () => { fn(3, 4) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail for abstractions with a wrong type definition" actual

    it "should resolve abstractions with many expressions" $ do
      let code = unlines
            [ "fn :: Float -> Float -> Boolean"
            , "fn = (a, b) => {"
            , "  sum = a + b"
            , "  moreThanTen = sum > 10"
            , "  computed = moreThanTen ? sum * 2 : sum / 2"
            , "  return computed / 2 == 0"
            , "}"
            , ""
            , "main = () => { fn(3, 4) }"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve abstractions with many expressions" actual

    ---------------------------------------------------------------------------


    -- If/Else:

    it "should infer a simple if else expression" $ do
      let code   = unlines
                    [ "main = () => {"
                    , "  if (true) {"
                    , "    \"OK\""
                    , "  }"
                    , "  else {"
                    , "    \"NOT OK\""
                    , "  }"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer a simple if else expression" actual

    it "should fail to infer an if else expression if the condition is not a Boolean" $ do
      let code   = unlines
                    [ "main = () => {"
                    , "  if (\"true\") {"
                    , "    \"OK\""
                    , "  }"
                    , "  else {"
                    , "    \"NOT OK\""
                    , "  }"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail to infer an if else expression if the condition is not a Boolean" actual

    it "should fail to infer an if else expression if the type of if and else cases does not match" $ do
      let code   = unlines
                    [ "main = () => {" 
                    , "  if (true) {"
                    , "    \"OK\""
                    , "  }"
                    , "  else {"
                    , "    1"
                    , "  }"
                    , "}"
                    ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail to infer an if else expression if the type of if and else cases does not match" actual

    it "should infer a ternary expression" $ do
      let code   = unlines ["main = () => { true ? \"OK\" : \"NOT OK\" }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer a ternary expression" actual

    ---------------------------------------------------------------------------


    -- Pattern matching:

    it "should resolve where with a Boolean literal" $ do
      let code   = unlines ["x = where(true) {", "  true => \"OK\"", "  false => \"NOT OK\"", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should resolve where with a Boolean literal" actual

    it "should resolve where with a Integer input" $ do
      let
        code = unlines
          ["x = where(42) {", "  1 => \"NOPE\"", "  3 => \"NOPE\"", "  33 => \"NOPE\"", "  42 => \"YEAH\"", "}"]
        actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should resolve where with a Integer input" actual

    it "should resolve where with a string input" $ do
      let code =
            unlines ["x = where(\"42\") {", "  \"1\" => 1", "  \"3\" => 3", "  \"33\" => 33", "  \"42\" => 42", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should resolve where with a string input" actual

    it "should resolve where with an ADT that has unary constructors" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , ""
            , "main = () => {"
            , "  perhaps = Just(4)"
            , "  where(perhaps) {"
            , "    Just(a) => a"
            , "    Nothing => 0"
            , "  }"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve where with an ADT that has unary constructors" actual

    it "should fail to resolve a pattern when the pattern constructor does not match the ADT" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , "type Failure = Nope"
            , ""
            , "main = () => {"
            , "  perhaps = Nope"
            , "  where(perhaps) {"
            , "    Just(a) => a"
            , "    Nothing => 0"
            , "  }"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail to resolve a pattern when the pattern constructor does not match the ADT" actual

    it "should fail to resolve a pattern when the pattern constructor does not match the constructor arg types" $ do
      let code = unlines
            [ "type User = LoggedIn(String, Integer)"
            , ""
            , "main = () => {"
            , "  u = LoggedIn(\"John\", 33)"
            , "  where(u) {"
            , "    LoggedIn(Integer, x) => x"
            , "  }"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest
        "should fail to resolve a pattern when the pattern constructor does not match the constructor arg types"
        actual

    it "should fail to resolve a constructor pattern with different type variables applied" $ do
      let code = unlines
            [ "type User a = LoggedIn(a, Integer)"
            , ""
            , "main = () => {"
            , "  u = LoggedIn(\"John\", 33)"
            , "  where(u) {"
            , "    LoggedIn(Integer, x) => x"
            , "    LoggedIn(String, x) => x"
            , "  }"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail to resolve a constructor pattern with different type variables applied" actual

    it "should fail to resolve if the given constructor does not exist" $ do
      let code   = unlines ["x = where(3) {", "  LoggedIn(Integer, x) => x", "  LoggedIn(String, x) => x", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should fail to resolve if the given constructor does not exist" actual

    it "should resolve basic patterns for lists" $ do
      let code = unlines
            [ "a = where([1, 2, 3, 5, 8]) {"
            , "  [1, 2, 3] => 1"
            , "  [1, 2, n] => n"
            , "  [n, 3] => n"
            , "  [x, y, z] => x + y + z"
            , "}"
            ]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should resolve basic patterns for lists" actual

    it "should fail to resolve patterns of different types for list items" $ do
      let code   = unlines ["x = where([1, 2, 3, 5, 8]) {", "  [1, 2, 3] => 1", "  [\"1\", n] => n", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should fail to resolve patterns of different types for list items" actual

    it "should allow deconstruction of lists" $ do
      let code   = unlines ["x = where([1, 2, 3, 5, 8]) {", "  [1, 2, ...rest] => rest", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should allow deconstruction of lists" actual

    it "should correctly infer types of record pattern when the input has a variable type" $ do
      let code   = unlines ["fn2 = (a) => where(a) {", "  { z: z } => z", "  { x: x } => x", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should correctly infer types of record pattern when the input has a variable type" actual

    it "should correctly infer constructor patterns given a var" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , ""
            , "fn = (b) =>"
            , "  where(b) {"
            , "    Just(x) => x"
            , "  }"
            , ""
            , "main = () => { fn(Just(3)) }"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should correctly infer constructor patterns given a var" actual

    it "should correctly infer shorthand syntax for record property matching" $ do
      let code   = unlines ["fn = (r) => (", "  where(r) {", "    { x, y } => x + y", "  }", ")"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should correctly infer shorthand syntax for record property matching" actual

    it "should resolve ADTs with 3 parameters in is" $ do
      let code   = unlines ["export type Wish e a c = Wish(e, a, c)", "content = where(Wish(1, 2, 3)) {", "  Wish(_, _, c) => c", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should resolve ADTs with 3 parameters in is" actual

    it "should fail to resolve patterns for namespaced ADTs that do not exist" $ do
      let code   = unlines ["might = (x) => where(x) {", "  M.Maybe(a) => a", "}"]
          actual = unsafePerformIO $ inferModuleWithoutMain code
      snapshotTest "should fail to resolve patterns for namespaced ADTs that do not exist" actual

    it "should fail to resolve patterns for namespaced ADTs that do not exist when the namespace exists" $ do
      let codeA  = ""
          codeB  = unlines ["import M from \"./ModuleA\"", "", "might = (x) => where(x) {", "  M.Maybe(a) => a", "}"]
          actual = unsafePerformIO $ inferManyModulesWithoutMain "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should fail to resolve patterns for namespaced ADTs that do not exist when the namespace exists"
                   actual

    ---------------------------------------------------------------------------


    -- Imports:

    it "should resolve names from imported modules" $ do
      let codeA  = unlines ["export inc = (a) => (a + 1)", "", "add = (a, b) => (a + b)", "addThree = add(3)"]
          codeB  = unlines ["import { inc } from \"./ModuleA\"", "main = () => { inc(3) }"]
          actual = unsafePerformIO $ inferManyModules "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should resolve names from imported modules" actual

    it "should resolve namespaced imports" $ do
      let codeA  = "export singleton = (a) => ([a])"
          codeB  = unlines ["import L from \"./ModuleA\"", "main = () => { L.singleton(3) }"]
          actual = unsafePerformIO $ inferManyModules "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should resolve namespaced imports" actual

    it "should resolve usage of exported names" $ do
      let code   = unlines ["export inc = (a) => a + 1", "main = () => { inc(3) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve usage of exported names" actual

    it "should resolve usage of exported typed names" $ do
      let code   = unlines ["inc :: Integer -> Integer", "export inc = (a) => (a + 1)", "main = () => { inc(3) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve usage of exported typed names" actual

    it "should resolve ADT typings without vars" $ do
      let codeA  = "export type Something = Something"
          codeB  = unlines ["import S from \"./ModuleA\"", "fn :: S.Something -> S.Something", "export fn = (x) => x"]
      let actual = unsafePerformIO $ inferManyModulesWithoutMain "./ModuleB.mad" (M.fromList [("./ModuleB.mad", codeB), ("./ModuleA.mad", codeA)])
      snapshotTest "should resolve ADT typings without vars" actual

    ---------------------------------------------------------------------------


    -- Pipe operator:

    it "should resolve the pipe operator" $ do
      let code   = unlines ["inc = (a) => (a + 1)", "main = () => { 3 |> inc }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the pipe operator" actual

    ---------------------------------------------------------------------------


    -- Boolean operators:

    it "should resolve the and operator" $ do
      let code   = "main = () => { true && false }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the and operator" actual

    it "should resolve the or operator" $ do
      let code   = "main = () => { true || false }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the or operator" actual

    it "should resolve the combination of and and or operators" $ do
      let code   = "main = () => { true || false && true }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the combination of and and or operators" actual

    it "should resolve the gt operator" $ do
      let code   = "main = () => { 1 > 3 }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the gt operator" actual

    it "should resolve the lt operator" $ do
      let code   = "main = () => { 1 < 3 }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the lt operator" actual

    it "should resolve the gte operator" $ do
      let code   = "main = () => { 1 >= 3 }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the gte operator" actual

    it "should resolve the lte operator" $ do
      let code   = "main = () => { 1 <= 3 }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the lte operator" actual

    it "should resolve the negation operator" $ do
      let code   = "main = () => { !false }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve the negation operator" actual

    ---------------------------------------------------------------------------


    -- Typed expressions:

    it "should validate correct type annotations" $ do
      let code = unlines
            [ "inc :: Integer -> Integer"
            , "inc = (a) => (a + 1)"
            , ""
            , "type Maybe a = Just(a) | Nothing"
            , ""
            , "main = () => {"
            , "  (3 :: Integer)"
            , "  (Nothing :: Maybe a)"
            -- TODO: The surrounded parens are necessary for now as the grammar is too ambiguous.
            -- We need to split the production and reconnect it when building the canonical AST.
            , "  (Just(3) :: Maybe Integer)"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should validate correct type annotations" actual

    it "should validate type annotations and instantiate their variables" $ do
      let code = unlines
            [ "map :: (a -> b) -> List a -> List b"
            , "map = (f, xs) => (#- some JS -#)"
            , ""
            , "main = () => {"
            , "  [[1, 2], [3, 4]]"
            , "    |> map(map((x) => (x * 2)))"
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should validate type annotations and instantiate their variables" actual

    it "should validate type annotations for ADTs that have no param" $ do
      let code   = unlines ["type X = X", "main = () => { x = (X :: X) }"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should validate type annotations for ADTs that have no param" actual

    it "should parse functions as args for adts in typings correctly" $ do
      let code = unlines
            [ "map :: (a -> b) -> List a -> List b"
            , "map = (f, xs) => #- some JS -#"
            , "concat :: List a -> List a -> List a"
            , "export concat = (xs1, xs2) => (#- xs1.concat(xs2) -#)"
            , "reduce :: (a -> b -> a) -> a -> List b -> a"
            , "export reduce = (f, initial, xs) => (#- xs.reduce(f, initial) -#)"
            , ""
            , "ap :: List (a -> b) -> List a -> List b"
            , "export ap = (fns, xs) => reduce("
            , "  (agg, fn) => pipe("
            , "    map(fn),"
            , "    concat(agg)"
            , "  )(xs),"
            , "  [],"
            , "  fns"
            , ")"
            , ""
            , "main = () => {}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should parse functions as args for adts in typings correctly" actual

    ---------------------------------------------------------------------------



    -- Recursion:

    it "should resolve recursive functions" $ do
      let code   = unlines ["fn = (x) => x + fn(x)", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve recursive functions" actual

    it "should resolve fibonacci recursive function" $ do
      let code   = unlines ["fib = (n) => if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve fibonacci recursive function" actual

    ---------------------------------------------------------------------------



    -- Template Strings:

    it "should resolve template strings" $ do
      let code = unlines
            [ "x = if(true) { \"it is true\" } else { \"it is false\" }"
            , ""
            , "main = () => {"
            , "  `probably ${x}!`"
            , "  `3 + 7 is ${if(3 + 7 > 10) { \"more than 10\" } else { \"less than 10\" } }`" -- With some more complex interpolated things
            , "}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should resolve template strings" actual

    it "should fail to solve template strings when interpolated expressions are not strings" $ do
      let code   = "main = () => { `${4 + 3}!` }"
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail to solve template strings when interpolated expressions are not strings" actual

    -- Scope

    it "should figure out illegal recursive accesses" $ do
      let code   = unlines ["x = x + 1", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should figure out illegal recursive accesses" actual

    it "should figure out illegal recursive accesses within function bodies" $ do
      let code   = unlines ["g = (x) => {", "  p = p + 1", "", "  return p", "}", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should figure out illegal recursive accesses within function bodies" actual

    it "should fail when accessing a function without typing that is defined after" $ do
      let code   = unlines ["f = (x) => definedAfter(x)", "definedAfter = (x) => x + 1", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when accessing a function without typing that is defined after" actual

    it "should fail when accessing a executing an expression declared later" $ do
      let code   = unlines ["a = definedAfter(3)", "definedAfter :: Integer -> Integer", "definedAfter = (x) => x + 1", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when accessing a executing an expression declared later" actual

    it "should fail when shadowing a name even if it's defined after the function" $ do
      let code   = unlines ["g = (x) => {", "  a = 2", "  return a", "}", "a = 4", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when shadowing a name even if it's defined after the function" actual

    it "should fail when exporting a name not defined yet" $ do
      let code   = unlines ["export definedAfter", "definedAfter :: Integer -> Integer", "definedAfter = (x) => x + 1", "main = () => {}"]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should fail when exporting a name not defined yet" actual

    it "should infer most general type for inner lambdas" $ do
      let code   = unlines
            [ "export repeatWith = (f, count) => {"
            , "  helper = (index) => index >= count ? [] : [f(index), ...helper(index + 1)]"
            , ""
            , "  return helper(0)"
            , "}"
            , ""
            , "main = () => {}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should infer most general type for inner lambdas" actual

    it "should correctly find types in complex record expressions" $ do
      let code = unlines
            [ "reduce :: (a -> b -> a) -> a -> List b -> a"
            , "reduce = ???"
            , ""
            , "alias StepState = { bets :: List Integer, players :: List Integer }"
            , ""
            , "judgeByWeight :: a -> StepState -> List a -> Integer"
            , "judgeByWeight = ???"
            , ""
            , "export runBets = (state) => {"
            , "  // players = state.players"
            , "  return reduce("
            , "    (stepState, player) => {"
            , "      return pipe("
            , "        .bets,"
            , "        judgeByWeight(player, stepState),"
            , "        (bet) => ({ ...stepState, bets: [...stepState.bets, bet], players: stepState.players }),"
            , "      )(stepState)"
            , "    },"
            , "    { ...state, players: [] },"
            , "    state.players,"
            , "  )"
            , "}"
            , ""
            , "main = () => {}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should correctly find types in complex record expressions" actual

    it "should not alter initial record type when extending a record with new fields" $ do
      let code = unlines
            [ "getBackpointsFrom :: { x :: Float, y :: Float } -> Array { x :: Float, y :: Float } -> Array { x :: Float, y :: Float }"
            , "getBackpointsFrom = ???"
            , ""
            , "fromList :: List a -> Array a"
            , "fromList = ???"
            , ""
            , "toList :: Array a -> List a"
            , "toList = ???"
            , ""
            , "weld :: Array a -> Array a -> Array a"
            , "weld = ???"
            , ""
            , "chain :: (a -> List b) -> List a -> List b"
            , "chain = ???"
            , ""
            , "drawShadows = (shader, lightPosition) => {"
            , "  casterPoints = fromList(["
            , "    { x: 300, y: 200 },"
            , "    { x: 350, y: 200 },"
            , "    { x: 350, y: 250 },"
            , "    { x: 300, y: 250 },"
            , "  ])"
            , ""
            , "  backPoints :: Array { x :: Float, y :: Float }"
            , "  backPoints = getBackpointsFrom(lightPosition, casterPoints)"
            , "  first = ["
            , "    { ...backPoints[0], z: 1 },"
            , "    { ...backPoints[0], z: 0 },"
            , "  ]"
            , "  points = pipe("
            , "    toList,"
            , "    chain((p) => [{ x: p.x, y: p.y, z: 1 }, { x: p.x, y: p.y, z: 0 }]),"
            , "    fromList,"
            , "    weld($, fromList(first))"
            , "  )(backPoints)"
            , "}"
            , ""
            , "main = () => {}"
            ]
          actual = unsafePerformIO $ inferModule code
      snapshotTest "should not alter initial record type when extending a record with new fields" actual
