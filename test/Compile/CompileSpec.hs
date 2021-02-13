module Compile.CompileSpec where

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
import           Control.Monad.State
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import qualified AST.Optimized                 as Opt
import           Infer.Exp
import           Infer.Env                     as Infer
import           Infer.Infer
import           Optimize.Optimize
import           Error.Error
import           Explain.Reason
import           Parse.AST                     as Parse
import           Compile.Compile
import           Prelude                 hiding ( readFile )
import           GHC.IO                         ( unsafePerformIO )
import           Utils.PathUtils
import           TestUtils
import           Canonicalize.Canonicalize     as Can
import           Canonicalize.AST              as Can
import           Canonicalize.Env              as Can
import           Target
import           Infer.AST
import           Debug.Trace


snapshotTest :: String -> String -> Golden Text
snapshotTest name actualOutput = Golden
  { output        = pack actualOutput
  , encodePretty  = unpack
  , writeToFile   = T.writeFile
  , readFromFile  = T.readFile
  , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
  , directory     = ".snapshots"
  , failFirstTime = False
  }


-- TODO: Refactor in order to use the inferAST function instead that supports imports
tester :: Bool -> String -> String
tester optimized code =
  let Right ast    = buildAST "path" code
      table        = M.singleton "path" ast
      Right table' = runCanonicalization TNode Can.initialEnv table "path"
      Right canAST = Can.findAST table' "path"
      inferred     = runEnv canAST >>= (`runInfer` canAST)
  in  case inferred of
        Right x -> compile
          (CompilationConfig "/"
                             "/module.mad"
                             "/module.mad"
                             "./build"
                             False
                             optimized
                             TNode
          )
          (evalState (optimize optimized x) initialOptimizationState :: Opt.AST)
        Left e -> ppShow e
 where
  runEnv x = fst <$> runExcept
    (runStateT (buildInitialEnv Infer.initialEnv x) Unique { count = 0 })

coverageTester :: String -> String
coverageTester code =
  let Right ast    = buildAST "path" code
      table        = M.singleton "path" ast
      Right table' = runCanonicalization TNode Can.initialEnv table "path"
      Right canAST = Can.findAST table' "path"
      inferred     = runEnv canAST >>= (`runInfer` canAST)
  in  case inferred of
        Right x -> compile
          (CompilationConfig "/"
                             "/module.mad"
                             "/module.mad"
                             "./build"
                             True
                             False
                             TNode
          )
          (evalState (optimize False x) initialOptimizationState :: Opt.AST)
        Left e -> ppShow e
 where
  runEnv x = fst <$> runExcept
    (runStateT (buildInitialEnv Infer.initialEnv x) Unique { count = 0 })

tableTester :: FilePath -> Src.Table -> Src.AST -> String
tableTester rootPath table ast@Src.AST { Src.apath = Just path } =

  let
    canTable = case runCanonicalization TNode Can.initialEnv table path of
      Right table -> table
      Left  err   -> trace ("ERR: " <> ppShow err) mempty
    Right canAST = Can.findAST canTable path
    resolved =
      fst <$> runExcept
        (runStateT (solveTable canTable canAST) Unique { count = 0 })
  in
    case resolved of
      Right x ->
        concat
          $   compile
                (CompilationConfig rootPath path path "./build" False False TNode)
          .   (\a ->
                (evalState (optimize False a) initialOptimizationState :: Opt.AST)
              )
          <$> M.elems x
      Left e -> ppShow e

mainCompileFixture :: String
mainCompileFixture = unlines
  [ "export fn = (b, c) => (b + c)"
  , "inc :: Number -> Number"
  , "inc = (x) => (x + 1)"
  , "dec :: Number -> Number"
  , "dec = (x) => (x - 1)"
  , "double :: Number -> Number"
  , "double = (x) => (x * 2)"
  , "half :: Number -> Number"
  , "half = (x) => (x / 2)"
  , "3 |> half |> double"
  , "3 == 5"
  , "response     = { users: [] }"
  , "users        = response.users"
  , "carResponse  = { cars: [] }"
  , "allResponses = { ...response, ...carResponse }"
  , "where(allResponses) {"
  , "  is { users: a, cars: cs }: cs"
  , "  is _                     : []"
  , "}"
  , "where(3) {"
  , "  is Number: 3"
  , "}"
  , "where(\"3\") {"
  , "  is String: 3"
  , "}"
  , "where(true) {"
  , "  is Boolean: 3"
  , "}"
  , "where(3) {"
  , "  is 3: 3"
  , "}"
  , "where(\"3\") {"
  , "  is \"3\": 3"
  , "}"
  , "where(true) {"
  , "  is true: 3"
  , "}"
  , "log :: a -> a"
  , "log = (a) => (#- { console.log(a); return a; } -#)"
  , "if (true) { \"OK\" } else { \"NOT OK\" }"
  , "data Maybe a = Just a | Nothing"
  , "mapMaybe :: (a -> b) -> Maybe a -> Maybe b"
  , "mapMaybe = (f, m) => (where(m) {"
  , "  is Just x : Just(f(x))"
  , "  is Nothing: Nothing"
  , "})"
  , "might = Just(3)"
  , "x = where(might) {"
  , "  is Just a : a"
  , "  is Nothing: 1"
  , "}"
  , "true && false"
  , "false || true"
  , "false || true && false"
  , "1 > 3"
  , "1 < 3"
  , "(1 > 3) && (3 < 1) || true"
  , "1 >= 3"
  , "1 <= 3"
  , "(1 >= 3) && (3 <= 1) || true"
  , "!false"
  , "!true"
  , "!false && !true"
  , "!false || !true"
  , "arr = [1, 2, 3]"
  , "all = [ ...arr, 4, 5, 6]"
  , "where([1, 2, 3, 5, 8]) {"
  , "  is [1, 2, 3]: 1"
  , "  is [1, 2, n]: n"
  , "  is [n, 3]   : n"
  , "  is [x1, y1, z1]: x1 + y1 + z1"
  , "  is []       : 0"
  , "}"
  , "map :: (a -> b) -> List a -> List b"
  , "export map = (f, xs) => (where(xs) {"
  , "  is [a, b, c]: [f(a), ...map(f, [b, c])]"
  , "  is [a, b]   : [f(a), ...map(f, [b])]"
  , "  is [a]      : [f(a)]"
  , "  is []       : []"
  , "})"
  , "true"
  , "  ? \"ok\""
  , "  : \"not ok\""
  , "(1 == 2 ? \"ok\" : \"not ok\")"
  , "  |> (x) => (x)"
  , "  |> (x) => (x == \"ok\" ? 1 : 10)"
  , "  |> (x) => (x)"
  , "1 == 2 ? \"ok\" : \"not ok\""
  , "where(3)"
  , "  is 3: 48"
  , "  is n: 1 |> (x) => (x + 1)"
  , "(where(\"3\")"
  , "  is \"3\": 48"
  , "  is n: 1"
  , ") |> (x) => (x + 1)"
  , "where([1, 2, 3, 4, 5]) {"
  , "  is [2, ...rest]      : rest"
  , "  is [1, 2, 3, ...rest]: rest"
  , "} |> (x) => (x)"
  , "where({ x: 4, name: \"John\" }) {"
  , "  is { name: \"Bob\" }: \"Bob\""
  , "  is { x: x1, ...b } : b.name"
  , "}"
  , "addXAndY = (r) => ("
  , "  where(r) {"
  , "    is { x: x1, y }: x1 + y"
  , "  }"
  , ")"
  , "fnTCHOU = (x) => (x.a.b.c.d.e)"
  , "crazyRecordPattern = (r) => ("
  , "  where(r) {"
  , "    is { x: { y: { y: y }, ...k }, ...c }: y + k.z + c.o + c.i"
  , "  }"
  , ")"
  , ""
  , "tup = <1, 2, 3>"
  , ""
  , "<1, 2, 3> == <1, 2, 3>"
  , ""
  , "where(<1, 2>) {"
  , "  is <a, b>: a + b"
  , "}"
  , ""
  , "fn :: Number -> <Number, Number>"
  , "fn = (a) => (<a, a>)"
  , ""
  , "fst :: <a, b> -> a"
  , "fst = (tuple) => (where(tuple)"
  , "  is <a, _>: a"
  , ")"
  , ""
  , "snd = (tuple) => {"
  , "  b = where(tuple)"
  , "    is <_, b1>: b1"
  , "  return b"
  , "}"
  , ""
  , "fst(<1, 2>)"
  , "snd(<1, 2>)"
  , ""
  , " where(<Just(3), Just(4)>)"
  , "   is <Just n, Just m>: n + m"
  ]


monadTransformersProgram :: String
monadTransformersProgram = unlines
  [ "interface Semigroup a {"
  , "  assoc :: a -> a -> a"
  , "}"
  , ""
  , "interface Semigroup w => Monoid w {"
  , "  mempty :: w"
  , "  mappend :: w -> w -> w"
  , "}"
  , ""
  , "instance Semigroup (List a) {"
  , "  assoc = (xs1, xs2) => (#- xs1.concat(xs2) -#)"
  , "}"
  , ""
  , "instance Monoid (List a) {"
  , "  mempty = []"
  , "  mappend = (xs1, xs2) => (#- xs1.concat(xs2) -#)//assoc"
  , "}"
  , ""
  , "interface Functor m {"
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
  , "interface Monad m => MonadTrans m t {"
  , "  lift :: m a -> t m a"
  , "}"
  , ""
  , ""
  , "andDo :: Monad m => m b -> m a -> m b"
  , "export andDo = (b, a) => chain((_) => b, a)"
  , ""
  , ""
  , "export data WriterT w m a = WriterT (m <a, w>)"
  , ""
  , ""
  , "runWriterT :: WriterT w m a -> m <a, w>"
  , "export runWriterT = where is WriterT m: m"
  , ""
  , "liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c"
  , "liftA2 = (f, x1, x2) => ap(map(f, x1), x2)"
  , ""
  , "instance Functor m => Functor (WriterT w m) {"
  , "  map = (f, m) => WriterT(map(where is <a, w>: <f(a), w>, runWriterT(m)))"
  , "}"
  , ""
  , "instance (Applicative m, Monoid w) => Applicative (WriterT w m) {"
  , "  pure = (x) => WriterT(pure(<x, mempty>))"
  , ""
  , "  ap = (mf, mm) => WriterT(liftA2((x1, x2) => where(x1)"
  , "    is <a, w>: where(x2) is <b, ww>: <a(b), mappend(w, ww)>"
  , "    , runWriterT(mf), runWriterT(mm)))"
  , "}"
  , ""
  , "instance (Monoid w, Monad m) => Monad (WriterT w m) {"
  , "  of = pure"
  , ""
  , "  chain = (f, m) => WriterT("
  , "    chain("
  , "      where is <a, w>: chain("
  , "        where is <b, ww>: of(<b, mappend(w, ww)>)"
  , "        , runWriterT(f(a)))"
  , "      , runWriterT(m))"
  , "  )"
  , "}"
  , ""
  , "instance (Monad m, Monoid w) => MonadTrans m (WriterT w) {"
  , "  lift = (m) => WriterT(chain((a) => of(<a, mempty>), m))"
  , "}"
  , ""
  , "export data Identity a = Identity a"
  , ""
  , "run :: Identity a -> a"
  , "export runIdentity = where"
  , "  is Identity a: a"
  , ""
  , "instance Functor Identity {"
  , "  map = (f, m) => Identity(f(runIdentity(m)))"
  , "}"
  , ""
  , "instance Applicative Identity {"
  , "  pure = Identity"
  , ""
  , "  ap = (mf, mm) => Identity(runIdentity(mf)(runIdentity(mm)))"
  , "}"
  , ""
  , "instance Monad Identity {"
  , "  of = pure"
  , ""
  , "  chain = (f, mm) => f(runIdentity(mm))"
  , "}"
  , ""
  , ""
  , "export data StateT s m a = StateT (s -> m <a, s>)"
  , ""
  , "runStateT :: StateT s m a -> s -> m <a, s>"
  , "export runStateT = (m) => where(m) is StateT f: (a) => f(a)"
  , ""
  , "instance Functor m => Functor (StateT s m) {"
  , "  map = (f, m) => StateT((s) =>"
  , "    map("
  , "      where is <a, ss>: <f(a), ss>,"
  , "      runStateT(m, s)"
  , "    )"
  , "  )"
  , "}"
  , ""
  , "instance Monad m => Applicative (StateT s m) {"
  , "  pure = (a) => StateT((s) => of(<a, s>))"
  , ""
  , "  ap = (mf, mm) => StateT("
  , "    (s) => chain("
  , "      where is <f, ss>: chain("
  , "        where is <m, sss>: of(<f(m), sss>),"
  , "        runStateT(mm)(ss)"
  , "      ),"
  , "      runStateT(mf)(s)"
  , "    )"
  , "  )"
  , "}"
  , ""
  , "instance Monad m => Monad (StateT s m) {"
  , "  of = (a) => StateT((s) => of(<a, s>))"
  , ""
  , "  chain = (f, m) => StateT("
  , "    (s) => chain("
  , "      where is <a, ss>: runStateT(f(a), ss),"
  , "      runStateT(m, s)"
  , "    )"
  , "  )"
  , "}"
  , ""
  , "instance Monad m => MonadTrans m (StateT s) {"
  , "  lift = (m) => StateT((s) => chain((a) => of(<a, s>), m))"
  , "}"
  , ""
  , "interface (Monoid w, Monad m) => MonadWriter w m {"
  , "  tell :: w -> m ()"
  , "}"
  , ""
  , "instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) {"
  , "  tell = (v) => WriterT(of(<(), v>))"
  , "}"
  , ""
  , "instance (Monoid w, Monad m, MonadWriter w m) => MonadWriter w (StateT s m) {"
  , "  tell = pipe(tell, lift)"
  , "}"
  , ""
  , "interface Monad m => MonadState s m {"
  , "  put :: s -> m ()"
  , "  get :: m s"
  , "  modify :: (s -> s) -> m ()"
  , "}"
  , ""
  , "instance Monad m => MonadState s (StateT s m) {"
  , "  put = (s) => StateT((_) => of(<(), s>))"
  , ""
  , "  get = StateT((s) => of(<s, s>))"
  , ""
  , "  modify = (f) => StateT((s) => of(<(), f(s)>))"
  , "}"
  , ""
  , "instance (Monoid w, Monad m, MonadState s m) => MonadState s (WriterT w m) {"
  , "  put = pipe(put, lift)"
  , "  "
  , "  get = lift(get)"
  , "  "
  , "  modify = pipe(modify, lift)"
  , "}"
  , ""
  , "alias Stack a = StateT Number (WriterT (List String) Identity) a"
  , ""
  , "hep :: MonadWriter w m => w -> m ()"
  , "hep = tell"
  , ""
  , "sumAndLog :: MonadWriter (List String) m => Number -> m Number"
  , "sumAndLog = pipe("
  , "  of,"
  , "  chain((x) => of(x + 18)),"
  , "  chain((x) => tell(['Summed 18']) |> andDo(of(x)))"
  , ")"
  , ""
  , "runStack :: Number -> Stack a -> <<a, Number>, List String>"
  , "runStack = (x, m) => pipe("
  , "  (m) => runStateT(m, x),"
  , "  runWriterT,"
  , "  runIdentity"
  , ")(m)"
  , ""
  , "of(3)"
  , "  |> chain((x) => of(29 * x))"
  , "  |> map((x) => x * 17)"
  , "  |> chain((_) => hep(['HOP']))"
  , "  |> chain((_) => hep(['HIP']))"
  , "  |> chain((_) => put(157))"
  , "  |> chain((_) => hep(['HAP']))"
  , "  |> andDo(of(5))"
  , "  |> chain(sumAndLog)"
  , "  |> runStack(37)"
  ]

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile to JS" $ do
      let actual = tester False mainCompileFixture
      snapshotTest "should compile to JS" actual

    it "should compile to JS with coverage trackers when COVERAGE_MODE is on"
      $ do
          let actual = coverageTester mainCompileFixture
          snapshotTest
            "should compile to JS with coverage trackers when COVERAGE_MODE is on"
            actual

    it "should compile interfaces and instances" $ do
      let code = unlines
            [ "data Maybe a = Just a | Nothing"
            , ""
            , "data Either e a = Right a | Left e"
            , ""
            , "interface Functor m {"
            , "  map :: (a -> b) -> m a -> m b"
            , "}"
            , ""
            , "instance Functor Maybe {"
            , "  map = (f) => where"
            , "    is Just x : Just(f(x))"
            , "    is Nothing: Nothing"
            , "}"
            , ""
            , "instance Functor List {"
            , "  map = (f) => where"
            , "    is [h, ...t]: [f(h), ...map(f, t)]"
            , "    is [l]      : [f(l)]"
            , "    is []       : []"
            , "}"
            , ""
            , "interface Monad m {"
            , "  chain :: (a -> m b) -> m a -> m b"
            , "  of :: a -> m a"
            , "}"
            , ""
            , "instance Monad Maybe {"
            , "  chain = (f) => where"
            , "    is Just x : f(x)"
            , "    is Nothing: Nothing"
            , ""
            , "  of = (x) => Just(x)"
            , "}"
            , ""
            , "instance Monad (Either e) {"
            , "  chain = (f) => where"
            , "    is Right x: f(x)"
            , "    is Left e : Left(e)"
            , ""
            , "  of = (x) => Right(x)"
            , "}"
            , ""
            , "inc = (a) => a + 1"
            , ""
            , "map(inc, [1, 2 ,3])"
            , ""
            , "chain((x) => Just(x + 1), Just(3))"
            , ""
            , "doIt :: Functor m => m Number -> m Number"
            , "doIt = (x) => map((k) => k + 1, x)"
            , ""
            , "doIt(Just(3))"
            , ""
            , "p = 1"
            , ""
            , "fn :: Functor m => m Number -> m Number"
            , "fn = map(inc)"
            , ""
            , "fn(Just(3))"
            , "fn([4, 5, 6])"
            , ""
            , "hideCall = (x) =>"
            , "  where(chain((a) => Just(a + 1), x))"
            , "    is Just 2: chain((a) => Right(a + 1), Right(2))"
            ]
          actual = tester False code
      snapshotTest "should compile interfaces and instances" actual

    it
        "should compile constrained instances and resolve their dictionary parameters"
      $ do
          let
            code = unlines
              [ "data Either e a = Right a | Left e"
              , ""
              , "interface Show a {"
              , "  show :: a -> String"
              , "}"
              , ""
              , "instance Show Boolean {"
              , "  show = (b) => b ? 'True' : 'False'"
              , "}"
              , ""
              , "instance Show Number {"
              , "  show = (n) => (#- new Number(n).toString() -#)"
              , "}"
              , ""
              , "instance (Show a, Show b) => Show <a, b> {"
              , "  show = where is <a, b>: '<' ++ show(a) ++ ', ' ++ show(b) ++ '>'"
              , "}"
              , ""
              , "show(<1, false>)"
              , ""
              , "instance (Show a, Show b, Show c) => Show <a, b, c> {"
              , "  show = where is <a, b, c>: '<' ++ show(a) ++ ', ' ++ show(b) ++ ', ' ++ show(c) ++ '>'"
              , "}"
              , ""
              , "instance (Show e, Show a) => Show (Either e a) {"
              , "  show = where"
              , "    is Right a: 'Right ' ++ show(a)"
              , "    is Left  e: 'Left ' ++ show(e)"
              , "}"
              , ""
              , "show((Right(3) :: Either Number Number))"
              , ""
              , "fnWithConstraint :: Show a => a -> String"
              , "fnWithConstraint = show"
              , ""
              , "show(<1, 1>)"
              , "show(<false, 42, true>)"
              ]
            actual = tester False code
          snapshotTest
            "should compile constrained instances and resolve their dictionary parameters"
            actual

    it "should compile monad transformers" $ do
      let actual = tester False monadTransformersProgram
      snapshotTest "should compile monad transformers" actual

    it "should compile monad transformers with optimized dictionaries" $ do
      let actual = tester True monadTransformersProgram
      snapshotTest
        "should compile monad transformers with optimized dictionaries"
        actual

    it "should compile imports and exports" $ do
      let codeA = "export singleton = (a) => ([a])"
          astA  = buildAST "./ModuleA" codeA

          codeB = unlines
            [ "import L from \"./ModuleA\""
            , "import { singleton } from \"./ModuleA\""
            , "L.singleton(3)"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester "" astTable b
      snapshotTest "should compile imports and exports" actual


    it "should compile imports and exports of Namespaced ADTs" $ do
      let
        codeA = unlines
                  [ "export data Maybe a = Just a | Nothing"
                  , "data NotExportedADT a = NotExportedADT a | StillNotExported"
                  ]
        astA  = buildAST "./ADTs" codeA

        codeB = unlines
          [ "import ADTs from \"./ADTs\""
          , "(ADTs.Just(3) :: ADTs.Maybe Number)"
          , "ADTs.Nothing"
          , "fn :: ADTs.Maybe (ADTs.Maybe Number) -> ADTs.Maybe (ADTs.Maybe Number)"
          , "export fn = (m) => (m)"
          , "fn2 :: ADTs.Maybe (ADTs.Maybe Number) -> Number"
          , "export fn2 = (m) => ("
          , "  where(m) {"
          , "    is ADTs.Just (ADTs.Just n): n"
          , "  }"
          , ")"
          ]
        astB   = buildAST "./Module" codeB
        actual = case (astA, astB) of
          (Right a, Right b) ->
            let astTable = M.fromList [("./Module", b), ("./ADTs", a)]
            in  tableTester "" astTable b

      snapshotTest "should compile imports and exports of Namespaced ADTs"
                   actual


    it
        "should compile and resolve imported modules that import namespaced imports"
      $ do
          let
            mainModule = unlines
              [ "import W from \"./Wish\""
              , "import B from \"./Binary\""
              , "import FS from \"./FileSystem\""
              , "import Http from \"./Http\""
              , "import IO from \"./IO\""
              , ""
              , "Http.get(\"https://github.com/open-sorcerers/madlib/archive/master.zip\")"
              , "  |> W.map((response) => (where(response) {"
              , "    is Http.Response { body: Http.BinaryBody d }: d"
              , "  }))"
              , "  |> W.map(FS.BinaryData)"
              , "  |> W.chain(FS.writeFile(\"./f.zip\"))"
              , "  |> W.fulfill(IO.log, IO.log)"
              ]

            httpModule = unlines
              [ "import W from \"./Wish\""
              , "import B from \"./Binary\""
              , ""
              , "export data Body"
              , "  = TextBody String"
              , "  | BinaryBody B.ByteArray"
              , ""
              , "export data Response = Response { body :: Body }"
              , ""
              , "get :: String -> W.Wish e Response"
              , "export get = (url) => (#- -#)"
              ]

            fileSystemModule = unlines
              [ "import W from \"./Wish\""
              , "import B from \"./Binary\""
              , ""
              , "export data Data"
              , "  = TextData String"
              , "  | BinaryData B.ByteArray"
              , ""
              , "writeFile :: String -> Data -> W.Wish e String"
              , "export writeFile = (path, d) => (#- -#)"
              ]

            binaryModule = unlines
              [ "export data ByteWord"
              , "  = Int8Bit a"
              , "  | Int16Bit a"
              , "  | Int32Bit a"
              , "export data ByteArray = ByteArray (List ByteWord)"
              ]

            wishModule = unlines
              [ "export data Wish e a = Wish ((e -> m) -> (a -> m) -> m)"
              , ""
              , "of :: a -> Wish e a"
              , "export of = (a) => (Wish((bad, good) => (good(a))))"
              , ""
              , "map :: (a -> b) -> Wish e a -> Wish e b"
              , "export map = (f, m) => ("
              , "  Wish((bad, good) => ("
              , "    where(m) {"
              , "      is Wish run: run(bad, (x) => (good(f(x))))"
              , "    }"
              , "  ))"
              , ")"
              , ""
              , "chain :: (a -> Wish f b) -> Wish e a -> Wish f b"
              , "export chain = (f, m) => ("
              , "  Wish((bad, good) => ("
              , "    where(m) {"
              , "      is Wish run1: run1(bad, (x) => ("
              , "        where(f(x)) {"
              , "          is Wish run2: run2(bad, good)"
              , "        }"
              , "      ))"
              , "    }"
              , "  ))"
              , ")"
              , ""
              , "fulfill :: (e -> m) -> (a -> m) -> Wish e a -> m"
              , "export fulfill = (bad, good, m) => (where(m) {"
              , "  is Wish run: run(bad, good)"
              , "})"
              ]

            ioModule = unlines
              [ "log :: a -> a"
              , "export log = (a) => (#- { console.log(a); return a; } -#)"
              ]


            files = M.fromList
              [ ("/root/project/src/Main.mad"      , mainModule)
              , ("/root/project/src/Http.mad"      , httpModule)
              , ("/root/project/src/Wish.mad"      , wishModule)
              , ("/root/project/src/Binary.mad"    , binaryModule)
              , ("/root/project/src/FileSystem.mad", fileSystemModule)
              , ("/root/project/src/IO.mad"        , ioModule)
              ]

            pathUtils = defaultPathUtils
              { readFile           = makeReadFile files
              , byteStringReadFile = makeByteStringReadFile files
              }

          let r = unsafePerformIO $ buildASTTable'
                pathUtils
                "/root/project/src/Main.mad"
                Nothing
                []
                "/root/project/src/Main.mad"
          let ast = r >>= flip Parse.findAST "/root/project/src/Main.mad"
          let actual = case (ast, r) of
                (Right a, Right t) -> tableTester "/root/project/src" t a
                (Left  e, _      ) -> ppShow e
                (_      , Left e ) -> ppShow e

          snapshotTest
            "should compile and resolve imported modules that import namespaced imports"
            actual


    it "should compile and resolve imported packages" $ do
      let
        madlibDotJSON = unlines ["{", "  \"main\": \"src/Main.mad\"", "}"]

        libMain       = unlines
          [ "import R from \"./Utils/Random\""
          , "export random = (seed) => (R.random(seed))"
          ]

        libRandom = "export random = (seed) => (seed / 2)"

        main      = unlines ["import R from \"random\"", "R.random(3)"]

        files     = M.fromList
          [ ("/madlib_modules/random/madlib.json"         , madlibDotJSON)
          , ("/madlib_modules/random/src/Main.mad"        , libMain)
          , ("/madlib_modules/random/src/Utils/Random.mad", libRandom)
          , ("/src/Main.mad"                              , main)
          ]

        pathUtils = defaultPathUtils
          { readFile = makeReadFile files
          , byteStringReadFile = makeByteStringReadFile files
          , doesFileExist = \f -> if f == "/madlib_modules/random/madlib.json"
                              then return True
                              else return False
          }

      let r = unsafePerformIO $ buildASTTable' pathUtils
                                               "/src/Main.mad"
                                               Nothing
                                               []
                                               "/src/Main.mad"

      let ast = r >>= flip Parse.findAST "/src/Main.mad"
      let actual = case (ast, r) of
            (Right a, Right t) -> tableTester "/src" t a

      snapshotTest "should compile and resolve imported packages" actual


    it
        "should compile and resolve imported packages when project is not at root path"
      $ do
          let
            madlibDotJSON = unlines ["{", "  \"main\": \"src/Main.mad\"", "}"]

            libMain       = unlines
              [ "import R from \"./Utils/Random\""
              , "export random = (seed) => (R.random(seed))"
              , "export data Maybe a = Just a | Nothing"
              ]

            libRandom = "export random = (seed) => (seed / 2)"

            main      = unlines ["import R from \"random\"", "R.random(3)"]

            files     = M.fromList
              [ ( "/root/project/madlib_modules/random/madlib.json"
                , madlibDotJSON
                )
              , ("/root/project/madlib_modules/random/src/Main.mad", libMain)
              , ( "/root/project/madlib_modules/random/src/Utils/Random.mad"
                , libRandom
                )
              , ("/root/project/src/Main.mad", main)
              ]

            pathUtils = defaultPathUtils
              { readFile           = makeReadFile files
              , byteStringReadFile = makeByteStringReadFile files
              , doesFileExist      = \f ->
                if f == "/root/project/madlib_modules/random/madlib.json"
                  then return True
                  else return False
              }

          let r = unsafePerformIO $ buildASTTable'
                pathUtils
                "/root/project/src/Main.mad"
                Nothing
                []
                "/root/project/src/Main.mad"

          let ast = r >>= flip Parse.findAST "/root/project/src/Main.mad"
          let actual = case (ast, r) of
                (Right a, Right t) -> tableTester "/root/project/src" t a

          snapshotTest
            "should compile and resolve imported packages when project is not at root path"
            actual


    it "should compile and resolve files importing prelude modules" $ do
      let
        listModule = unlines
          [ "map :: (a -> b) -> List a -> List b"
          , "export map = (f, xs) => (#- xs.map(f) -#)"
          ]

        main =
          unlines ["import L from \"List\"", "L.map((x) => (x * 2), [1, 2, 3])"]

        files = M.fromList
          [ ("/root/project/prelude/__internal__/List.mad", listModule)
          , ("/root/project/src/Main.mad"                 , main)
          ]

        pathUtils = defaultPathUtils
          { readFile           = makeReadFile files
          , byteStringReadFile = makeByteStringReadFile files
          , getExecutablePath  = return "/root/project/madlib"
          }

      let r = unsafePerformIO $ buildASTTable' pathUtils
                                               "/root/project/src/Main.mad"
                                               Nothing
                                               []
                                               "/root/project/src/Main.mad"

      let ast = r >>= flip Parse.findAST "/root/project/src/Main.mad"
      let actual = case (ast, r) of
            (Right a, Right t) -> tableTester "/root/project/src" t a
            (Left  e, _      ) -> ppShow e
            (_      , Left e ) -> ppShow e

      snapshotTest
        "should compile and resolve files importing prelude modules"
        actual

    it
        "should compile and resolve files importing modules that rely on type aliases"
      $ do
          let wishModule = unlines
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
                , "export data Wish e a = Wish ((e -> ()) -> (a -> ()) -> ())"
                , ""
                , ""
                , "instance Functor (Wish e) {"
                , "  map = (f, m) => Wish((bad, good) =>"
                , "    where(m)"
                , "      is Wish run: run(bad, (x) => (good(f(x))))"
                , "  )"
                , "}"
                , ""
                , "instance Applicative (Wish e) {"
                , "  pure = (a) => Wish((bad, good) => good(a))"
                , ""
                , "  ap = (mf, m) => Wish((bad, good) => where(<mf, m>)"
                , "    is <Wish runMF, Wish runM>:"
                , "      runM("
                , "        bad,"
                , "        (x) => runMF("
                , "          bad,"
                , "          (f) => good(f(x))"
                , "        )"
                , "      )"
                , "  )"
                , "}"
                , ""
                , "instance Monad (Wish e) {"
                , "  of = pure"
                , ""
                , "  chain = (f, m) => Wish((bad, good) =>"
                , "    where(m) "
                , "      is Wish run:"
                , "        run(bad, (x) =>"
                , "          where(f(x))"
                , "            is Wish r: r(bad, good)"
                , "        )"
                , "  )"
                , "}"
                , ""
                , ""
                , "mapRej :: (e -> f) -> Wish e a -> Wish f a"
                , "export mapRej = (f, m) => ("
                , "  Wish((bad, good) => ("
                , "    where(m) {"
                , "      is Wish run: run((x) => (bad(f(x))), good)"
                , "    }"
                , "  ))"
                , ")"
                , ""
                , ""
                , "chainRej :: (e -> Wish f b) -> Wish e a -> Wish f b"
                , "export chainRej = (f, m) => ("
                , "  Wish((bad, good) => ("
                , "    where(m) {"
                , "      is Wish run: run((x) => ("
                , "        where(f(x)) {"
                , "          is Wish r: r(bad, good)"
                , "        }"
                , "      ), good)"
                , "    }"
                , "  ))"
                , ")"
                , ""
                , ""
                , "good :: a -> Wish e a"
                , "export good = (a) => Wish((bad, good) => good(a))"
                , ""
                , "bad :: e -> Wish e a"
                , "export bad = (e) => ("
                , "  Wish((bad, good) => (bad(e)))"
                , ")"
                , ""
                , ""
                , "getWishFn = (w) => (where(w)"
                , "  is Wish fn: fn"
                , ")"
                , ""
                , ""
                , "parallel :: List (Wish e a) -> Wish e (List a)"
                , "export parallel = (wishes) => ("
                , "  Wish((bad, good) => (#- {"
                , "    const l = wishes.length"
                , "    let ko = false;"
                , "    let ok = 0;"
                , "    const out = new Array(l);"
                , "    const next = j => (j === l && good(out));"
                , "    const fork = (w, j) => (getWishFn(w)("
                , "      e => ko || (bad(e), ko = true),"
                , "      x => ko || (out[j] = x, next(++ok))"
                , "    ));"
                , "    wishes.forEach(fork);"
                , "  } -#))"
                , ")"
                , ""
                , ""
                , "fulfill :: (e -> f) -> (a -> b) -> Wish e a -> ()"
                , "export fulfill = (bad, good, m) => {"
                , "  where(m) {"
                , "    is Wish run: run(bad, good)"
                , "  }"
                , ""
                , "  return ()"
                , "}"
                ]

              main = unlines
                [ "import W from \"Wish\""
                , "of(3)"
                , "  |> map((x) => (x % 2))"
                , "  |> chain((x) => (of(x * 3)))"
                , "  |> chain((x) => (of(`finally a string`)))"
                , "  |> map((x) => (x ++ '!'))"
                , "  |> W.fulfill((a) => (()), (a) => (()))"
                ]

              files = M.fromList
                [ ("/root/project/prelude/__internal__/Wish.mad", wishModule)
                , ("/root/project/src/Main.mad"                 , main)
                ]

              pathUtils = defaultPathUtils
                { readFile           = makeReadFile files
                , byteStringReadFile = makeByteStringReadFile files
                , getExecutablePath  = return "/root/project/madlib"
                }

          let r = unsafePerformIO $ buildASTTable'
                pathUtils
                "/root/project/src/Main.mad"
                Nothing
                []
                "/root/project/src/Main.mad"

          let ast = r >>= flip Parse.findAST "/root/project/src/Main.mad"
          let actual = case (ast, r) of
                (Right a, Right t) -> tableTester "/root/project/src" t a
                (Left  e, _      ) -> ppShow e
                (_      , Left e ) -> ppShow e

          snapshotTest
            "should compile and resolve files importing modules that rely on type aliases"
            actual


    it "should compile and resolve imported packages that also rely on packages"
      $ do
          let
            mathMadlibDotJSON =
              unlines ["{", "  \"main\": \"src/Main.mad\"", "}"]
            mathMain = unlines ["export avg = (a, b) => ((a + b) / 2)"]

            randomMadlibDotJSON =
              unlines ["{", "  \"main\": \"src/Main.mad\"", "}"]

            randomMain = unlines
              [ "import R from \"./Utils/Random\""
              , "import M from \"math\""
              , "export random = (seed) => (R.random(seed) + M.avg(seed, seed))"
              ]

            libRandom = "export random = (seed) => (seed / 2)"

            main      = unlines ["import R from \"random\"", "R.random(3)"]

            files     = M.fromList
              [ ( "/madlib_modules/random/madlib_modules/math/madlib.json"
                , mathMadlibDotJSON
                )
              , ( "/madlib_modules/random/madlib_modules/math/src/Main.mad"
                , mathMain
                )
              , ("/madlib_modules/random/madlib.json", randomMadlibDotJSON)
              , ("/madlib_modules/random/src/Main.mad"        , randomMain)
              , ("/madlib_modules/random/src/Utils/Random.mad", libRandom)
              , ("/src/Main.mad"                              , main)
              ]

            pathUtils = defaultPathUtils
              { readFile           = makeReadFile files
              , byteStringReadFile = makeByteStringReadFile files
              , doesFileExist      = \f ->
                if f
                   == "/madlib_modules/random/madlib.json"
                   || f
                   == "/madlib_modules/random/madlib_modules/math/madlib.json"
                then
                  return True
                else
                  return False
              }

          let r = unsafePerformIO $ buildASTTable' pathUtils
                                                   "/src/Main.mad"
                                                   Nothing
                                                   []
                                                   "/src/Main.mad"

          let ast = r >>= flip Parse.findAST "/src/Main.mad"
          let actual = case (ast, r) of
                (Right a, Right t) -> tableTester "/src" t a

          snapshotTest
            "should compile and resolve imported packages that also rely on packages"
            actual

