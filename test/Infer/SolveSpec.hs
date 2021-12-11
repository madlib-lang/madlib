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
import           Infer.Exp
import           Infer.Env
import           Infer.Infer
import           Canonicalize.CanonicalM
import           Infer.AST
import           Error.Error
import           Parse.Madlib.AST
import           Canonicalize.Canonicalize
import           Run.Target
import           Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import           Data.Maybe
import Debug.Trace

snapshotTest :: Show a => String -> a -> Golden Text
snapshotTest name actualOutput = Golden { output        = pack $ ppShow actualOutput
                                        , encodePretty  = ppShow
                                        , writeToFile   = T.writeFile
                                        , readFromFile  = T.readFile
                                        , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
                                        , directory     = ".snapshots"
                                        , failFirstTime = False
                                        }

tester :: String -> Either CompilationError Slv.AST
tester code = do
  ast <- buildAST "path" code
  let table = M.singleton "path" ast
  ((table, _, _), _) <- runStateT (canonicalizeAST mempty TNode Can.initialEnv table "path")
                                  (CanonicalState { warnings = [], typesToDerive = mempty, derivedTypes = mempty })
  canAST <- Can.findAST table "path"

  runEnv canAST >>= (`runInfer` canAST)
  where runEnv x = fst <$> runExcept (runStateT (buildInitialEnv initialEnv x) InferState { count = 0, errors = [] })

tableTester :: Src.Table -> Src.AST -> Either CompilationError Slv.Table
tableTester table ast = do
  let astPath = fromMaybe "" $ Src.apath ast
  ((canTable, _, _), _) <- runStateT (canonicalizeAST mempty TNode Can.initialEnv table astPath)
                                     (CanonicalState { warnings = [], typesToDerive = mempty, derivedTypes = mempty })
  canAST <- Can.findAST canTable astPath

  let result = runExcept (runStateT (solveTable canTable canAST) InferState { count = 0, errors = [] })
  case result of
    Left e -> Left e

    Right (table', state) ->
      let errs      = errors state
          hasErrors = not (null errs)
      in  if hasErrors then Left (head errs) else Right table'

spec :: Spec
spec = do
  describe "infer" $ do
    it "should infer abstractions" $ do
      let code   = "(b, c) => (b + c)"
          actual = tester code
      snapshotTest "should infer abstractions" actual

    it "should infer assignments" $ do
      let code   = unlines ["fn = (b, c) => (b + c)", "fn(2, 3)"]
          actual = tester code
      snapshotTest "should infer assignments" actual

    it "should infer minus operator" $ do
      let code   = "(b, c) => (b - c)"
          actual = tester code
      snapshotTest "should infer minus operator" actual

    it "should infer multiplication operator" $ do
      let code   = "(b, c) => (b * c)"
          actual = tester code
      snapshotTest "should infer multiplication operator" actual

    it "should infer division operator" $ do
      let code   = "(b, c) => (b / c)"
          actual = tester code
      snapshotTest "should infer division operator" actual

    it "should infer equality operator" $ do
      let code   = "1 == 3"
          actual = tester code
      snapshotTest "should infer equality operator" actual

    it "should infer wrapped douleEq operator" $ do
      let code   = "((a, b) => (a == b))(1, 3)"
          actual = tester code
      snapshotTest "should infer wrapped douleEq operator" actual

    it "should infer an empty source" $ do
      let code   = ""
          actual = tester code
      snapshotTest "should infer an empty source" actual

    it "should fail for unbound variables" $ do
      let code   = "x"
          actual = tester code
      snapshotTest "should fail for unbound variables" actual

    ---------------------------------------------------------------------------


    -- ADTs:

    it "should infer adts" $ do
      let code   = unlines ["type Result = Success(String) | Error", "result = Success(\"response\")"]
          actual = tester code
      snapshotTest "should infer adts" actual

    it "should infer adts with type parameters" $ do
      let code   = unlines ["type Result a", "  = Success(a)", "  | Error", "result = Success(true)"]
          actual = tester code
      snapshotTest "should infer adts with type parameters" actual

    it "should infer application of adts" $ do
      let code = unlines
            [ "type Result = Success(String) | Error"
            , "result1 = Success(\"response\")"
            , "result2 = Error"
            , "((a, b) => (a == b))(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer application of adts" actual

    it "should infer adt return for abstractions" $ do
      let code = unlines
            [ "type Result a = Success(a) | Error"
            , "result1 = Success(\"response\")"
            , "result2 = Error"
            , "((a, b) => (a == b))(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer adt return for abstractions" actual

    it "should return an error when an ADT defines a type already existing" $ do
      let code   = unlines ["type Result a = Success a | Error", "type Result a = Success a | Error"]
          actual = tester code
      snapshotTest "should return an error when an ADT defines a type already existing" actual

    it "should fail to infer ADTs with constructors that have free vars" $ do
      let code   = unlines ["type Result = Success(b)", ""]
          actual = tester code
      snapshotTest "should fail to infer ADTs with constructors that have free vars" actual

    it "should infer adts with record constructors" $ do
      let code = unlines
            [ "type Result = Success({ value :: String }) | Error({ message :: String })"
            , "result1 = Success({ value: `42` })"
            , "result2 = Error({ message: 'Err' })"
            , "((a, b) => (a == b))(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer adts with record constructors" actual

    it "should infer params for adts" $ do
      let code =
            unlines ["type Result = Success({ value :: String })", "r = Success({ value: \"42\" })", "((a) => a)(r)"]
          actual = tester code
      snapshotTest "should infer params for adts" actual

    it "should fail if it uses an ADT not defined" $ do
      let code =
            unlines
              [ "addNegativeTen :: Maybe Integer -> Integer"
              , "export addNegativeTen = (a) => (a + -10)"
              , "addNegativeTen(3)"
              ]
          actual = tester code
      snapshotTest "should fail if it uses an ADT not defined" actual

    it "should fail if it uses an ADT not defined in patterns" $ do
      let code   = unlines ["where(3) {", "  NotExisting => 5", "}"]
          actual = tester code
      snapshotTest "should fail if it uses an ADT not defined in patterns" actual

    it "should resolve ADTs with function parameters" $ do
      let code   = unlines ["export type Wish e a = Wish ((e -> m) -> (a -> m) -> m)", "Wish((bad, good) => (good(3)))"]
          actual = tester code
      snapshotTest "should resolve ADTs with function parameters" actual

    it "should resolve namespaced ADTs in patterns" $ do
      let codeA = "export type Maybe a = Just(a) | Nothing"
          astA  = buildAST "./ModuleA" codeA
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
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
            (Left e, _     ) -> Left e
            (_     , Left e) -> Left e
      snapshotTest "should resolve namespaced ADTs in patterns" actual

    it "should resolve namespaced ADTs in other ADTs" $ do
      let
        codeA = "export type Maybe a = Just(a) | Nothing"
        astA  = buildAST "./ModuleA" codeA
        codeB =
          unlines ["import M from \"./ModuleA\"", "type MyType = MyType(M.Maybe String)", "x = MyType(M.Just(\"3\"))"]
        astB   = buildAST "./ModuleB" codeB
        actual = case (astA, astB) of
          (Right a, Right b) ->
            let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
          (Left e, _     ) -> Left e
          (_     , Left e) -> Left e
      snapshotTest "should resolve namespaced ADTs in other ADTs" actual

    it "should allow ADT constructors to have record parameters" $ do
      let
        codeA = "export type Point = Point(#[Integer, Integer])"
        astA  = buildAST "./ModuleA" codeA
        codeB = unlines
          ["import P from \"./ModuleA\"", "p = P.Point(#[2, 4])", "where(p) {", "  P.Point(#[a, b]) => a + b", "}"]
        astB   = buildAST "./ModuleB" codeB
        actual = case (astA, astB) of
          (Right a, Right b) ->
            let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
          (Left e, _     ) -> Left e
          (_     , Left e) -> Left e
      snapshotTest "should allow ADT constructors to have record parameters" actual

    ---------------------------------------------------------------------------



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
            , "export type Wish e a = Wish((e -> f) -> (a -> b) -> ())"
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
            , "fulfill :: (e -> f) -> (a -> b) -> Wish e a -> ()"
            , "export fulfill = (badCB, goodCB, m) => {"
            , "  where(m) {"
            , "    Wish(run) => run(badCB, goodCB)"
            , "  }"
            , ""
            , "  return ()"
            , "}"
            ]
          astA  = buildAST "./ModuleA" codeA
          codeB = unlines
            [ "import W from \"./ModuleA\""
            , ""
            , "double :: Functor f => f Integer -> f Integer"
            , "double = map((x) => x * 2)"
            , ""
            , "of(3)"
            , "  |> map((x) => (x + 3))"
            , "  |> chain((x) => (of(x * 3)))"
            , "  |> W.fulfill((a) => (()), (a) => (()))"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
            (Left e, _     ) -> Left e
            (_     , Left e) -> Left e
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
            , "read('3')"
            ]
          actual = tester code
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
            , "(read('3') :: Integer)"
            ]
          actual = tester code
      snapshotTest "should resolve ambiguity with type annotations" actual

    it "should resolve constrained instances" $ do
      let code = unlines
            [ "interface Show a {"
            , "  show :: a -> String"
            , "}"
            , ""
            , "instance Show Boolean {"
            , "  show = (b) => b ? 'True' : 'False'"
            , "}"
            , ""
            , "instance Show Integer {"
            , "  show = (n) => (#- new Integer(n).toString() -#)"
            , "}"
            , ""
            , "instance (Show a, Show b) => Show #[a, b] {"
            , "  show = where { #[a, b] => '#[' ++ show(a) ++ ', ' ++ show(b) ++ ']' }"
            , "}"
            , ""
            , "show(#[1, false])"
            ]
          actual = tester code
      snapshotTest "should resolve constrained instances" actual

    it "should fail for instances missing constrains" $ do
      let code = unlines
            [ "interface Show a {"
            , "  show :: a -> String"
            , "}"
            , ""
            , "instance Show Boolean {"
            , "  show = (b) => b ? 'True' : 'False'"
            , "}"
            , ""
            , "instance Show Integer {"
            , "  show = (n) => (#- new Integer(n).toString() -#)"
            , "}"
            , ""
            , "instance Show #[a, b] {"
            , "  show = where { #[a, b] => '#[' ++ show(a) ++ ', ' ++ show(b) ++ ']' }"
            , "}"
            , ""
            , "show(#[1, false])"
            ]
          actual = tester code
      snapshotTest "should fail for instances missing constrains" actual

    it "should fail when no instance is found" $ do
      let code = unlines
            [ "interface Show a {"
            , "  show :: a -> String"
            , "}"
            , ""
            , "instance Show Boolean {"
            , "  show = (b) => b ? 'True' : 'False'"
            , "}"
            , ""
            , "instance (Show a, Show b) => Show #[a, b] {"
            , "  show = where { #[a, b] => '#[' ++ show(a) ++ ', ' ++ show(b) ++ ']' }"
            , "}"
            , ""
            , "show(3)"
            ]
          actual = tester code
      snapshotTest "should fail when no instance is found" actual



    ---------------------------------------------------------------------------



    -- Records:

    it "should infer a record field access" $ do
      let code   = unlines ["a = { x: 3.1415, y: -500 }", "a.x"]
          actual = tester code
      snapshotTest "should infer a record field access" actual

    it "should infer an App with a record" $ do
      let code   = unlines ["a = { x: 3, y: 5 }", "xPlusY = (r) => (r.x + r.y)", "xPlusY(a)"]
          actual = tester code
      snapshotTest "should infer an App with a record" actual

    it "should fail to infer record if their fields do not match" $ do
      let code   = "{ x: 3, y: 5 } == { name: \"John\" }"
          actual = tester code
      snapshotTest "should fail to infer record if their fields do not match" actual

    it "should infer a record with a type annotation" $ do
      let code   = "({ x: 3, y: 7 } :: { x :: Integer, y :: Integer })"
          actual = tester code
      snapshotTest "should infer a record with a type annotation" actual

    it "should infer abstraction param that is a deep record" $ do
      let code   = "f = (x) => (x.a.b.c.d.e)"
          actual = tester code
      snapshotTest "should infer abstraction param that is a deep record" actual

    it "should infer abstraction param that have a record exp as body" $ do
      let code   = "addTodo = (state) => ({ ...state, x: \"3\", y: state.y })"
          actual = tester code
      snapshotTest "should infer abstraction param that have a record exp as body" actual

    it "should fail when spreading a non spreadable type into a record" $ do
      let code   = unlines ["{ ...3, x: 1 }"]
          actual = tester code
      snapshotTest "should fail when spreading a non spreadable type into a record" actual

    it "correctly infer various record transformations" $ do
      let code = unlines
            [ "ff = (record) => (["
            , "  ...record.x,"
            , "  ...record.z,"
            , "  ...record.y"
            , "])"
            , ""
            , ""
            , "fr1 = (x) => ({ ...x, p: 3 })"
            , ""
            , "fr2 = (r, x) => ({ ...r, p: x })"
            , ""
            , "r0 = fr1({ z: 9, p: 3 })"
            , "r1 = { ...fr1({ z: 9, p: 3 }), y: 5 }"
            , "r2 = fr2({ p: '4', g: 5 }, '5')"
            , ""
            , "fxy = (s, e) => ({"
            , "  ...e,"
            , "  c: s.x + 1,"
            , "  b: '3'"
            , "})"
            ]
          actual = tester code
      snapshotTest "correctly infer various record transformations" actual

    it "should infer complex where expressions with records" $ do
      let code = unlines
            [ "export alias ComparisonResult = Integer"
            , ""
            , "export MORE = 1"
            , "export LESS = -1"
            , "export EQUAL = 1"
            , ""
            , "interface Comparable a {"
            , "  compare :: a -> a -> ComparisonResult"
            , "}"
            , ""
            , "instance Comparable Integer {"
            , "  compare = (a, b) => a > b ? MORE : a == b ? EQUAL : LESS"
            , "}"
            , ""
            , "instance Comparable String {"
            , "  compare = (a, b) => #- a > b ? MORE : a == b ? EQUAL : LESS -#"
            , "}"
            , ""
            , "interface Functor m {"
            , "  map :: (a -> b) -> m a -> m b"
            , "}"
            , "instance Functor List {"
            , "  map = (f, xs) => #- xs.map((x) => f(x)) -#"
            , "}"
            , ""
            , "sortBy :: (a -> a -> ComparisonResult) -> List a -> List a"
            , "export sortBy = (fn, xs) => #- xs.sort((a, b) => fn(a)(b)) -#"
            , ""
            , "chain :: (a -> List b) -> List a -> List b"
            , "chain = #--#"
            , ""
            , "FunctionLink = where { f => 'moduleName' ++ f.name }"
            , "generateFunctionLinks = pipe("
            , "  chain(.expressions),"
            , "  sortBy((a, b) => where(#[a, b]) {"
            , "    #[{ name: nameA }, { name: nameB }] => compare(nameA, nameB)"
            , "    #[{ name: nameC }, { ik: nameD }] => compare(nameC, nameD)"
            , "    #[{ tchouk: nameD }, { name: nameC }] => compare(nameC, nameD)"
            , "    #[{ name: nameC }, { lui: nameD }] => compare(nameC, nameD)"
            , "    #[{ name: nameC }, { po: { pi: { nameD }}}] => compare(nameC, nameD)"
            , "    #[{ po: { pi: { nameC }}}, { name: nameD }] => compare(nameC, nameD)"
            , "  }),"
            , "  map(FunctionLink)"
            , ")"
            ]
          actual = tester code
      snapshotTest "should infer complex where expressions with records" actual

    it "should infer record params that are partially used in abstractions" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
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
            , "export prop = propOrWrappedDefault(Nothing, '')"
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
          actual = tester code
      snapshotTest "should infer record params that are partially used in abstractions" actual

    ---------------------------------------------------------------------------


    -- Lists:

    it "should infer list constructors" $ do
      let code   = unlines ["[]", "[1, 2, 3]", "[\"one\", \"two\", \"three\"]"]
          actual = tester code
      snapshotTest "should infer list constructors" actual

    it "should infer list spread" $ do
      let code   = unlines ["[ 1, ...[1, 2]]"]
          actual = tester code
      snapshotTest "should infer list spread" actual

    it "should fail when spreading an array of a different type" $ do
      let code   = unlines ["[ 1, ...[\"1\", \"2\"]]"]
          actual = tester code
      snapshotTest "should fail when spreading an array of a different type" actual

    it "should fail when spreading a non spreadable type into an array" $ do
      let code   = unlines ["[1, ...3]"]
          actual = tester code
      snapshotTest "should fail when spreading a non spreadable type into an array" actual

    it "should fail when constructing a list with different types" $ do
      let code   = unlines ["[1, false, 3, 4]"]
          actual = tester code
      snapshotTest "should fail when constructing a list with different types" actual

    -- Tuples

    it "should infer tuple constructors" $ do
      let code   = unlines ["#[1, 2, 3]", "#[true, \"John\", 33]"]
          actual = tester code
      snapshotTest "should infer tuple constructors" actual


    ---------------------------------------------------------------------------


    -- Applications:

    it "should fail for applications with a wrong argument type" $ do
      let code   = unlines ["fn = (a, b) => (a == b)", "fn(\"3\", 4)"]
          actual = tester code
      snapshotTest "should fail for applications with a wrong argument type" actual

    it "should infer applications where the abstraction results from an application" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
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
            , "  'alice', 'bob', 'caroline', 'david', 'elizabeth', 'frances', 'georgina', 'harold'"
            , "]"
            , "log = (a) => (#- { console.log(a); return a; } -#)"
            , "mash :: String -> String -> String"
            , "mash = (a, b) => (a ++ b)"
            , "mash('>>', 'shit') |> log"
            , "nth(2, names) |> log"
            , "flip(mash)('>>', 'shit') |> log"
            , "thn = flip(nth)"
            , "thn(names, 2) |> log"
            , "flip(nth)(names, 2) |> log"
            ]
          actual = tester code
      snapshotTest "should infer applications where the abstraction results from an application" actual

    it "should fail to infer applications when a variable is used with different types" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , ""
            , "fromMaybe :: a -> Maybe a -> a"
            , "fromMaybe = (a, maybe) => #- -#"
            , ""
            , "export type Wish e a = Wish((e -> f) -> (a -> b) -> ())"
            , ""
            , "of = (a) => Wish((bad, good) => good(a))"
            , ""
            , "nth :: Integer -> List a -> Maybe a"
            , "nth = (i, xs) => #- -#"
            , ""
            , "slice :: Integer -> Integer -> List a -> List a"
            , "export slice = (start, end, xs) => (#- xs.slice(start, end) -#)"
            , ""
            , "len :: List a -> Integer"
            , "export len = (xs) => (#- xs.length -#)"
            , ""
            , "export alias Action a = a -> String -> List (Wish (a -> a) (a -> a))"
            , ""
            , ""
            , "type Todo = Todo(String, Boolean)"
            , "alias State = { input :: String, todos :: List Todo }"
            , ""
            , "pipe("
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
            , "      todos: ["
            , "        ...slice(0, len(state.todos), state)"
            , "      ]"
            , "    })"
            , "  )(state))"
            , "]"
            ]
          actual = tester code
      snapshotTest "should fail to infer applications when a variable is used with different types" actual



    ---------------------------------------------------------------------------


    -- Abstractions:

    -- TODO: Write and implement the same test with ADTs
    -- TODO: Write tests where implementation and definition don't match to force
    -- implementing it as it's currently not implemented.
    it "should resolve abstractions with a type definition" $ do
      let code   = unlines ["fn :: Integer -> Integer -> Boolean", "fn = (a, b) => (a == b)", "fn(3, 4)"]
          actual = tester code
      snapshotTest "should resolve abstractions with a type definition" actual

    it "should fail for abstractions with a wrong type definition" $ do
      let code   = unlines ["fn :: String -> Integer -> Boolean", "fn = (a, b) => (a == b)", "fn(3, 4)"]
          actual = tester code
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
            , "fn(3, 4)"
            ]
          actual = tester code
      snapshotTest "should resolve abstractions with many expressions" actual

    ---------------------------------------------------------------------------


    -- If/Else:

    it "should infer a simple if else expression" $ do
      let code   = unlines ["if (true) {", "  \"OK\"", "}", "else {", "  \"NOT OK\"", "}"]
          actual = tester code
      snapshotTest "should infer a simple if else expression" actual

    it "should fail to infer an if else expression if the condition is not a Boolean" $ do
      let code   = unlines ["if (\"true\") {", "  \"OK\"", "}", "else {", "  \"NOT OK\"", "}"]
          actual = tester code
      snapshotTest "should fail to infer an if else expression if the condition is not a Boolean" actual

    it "should fail to infer an if else expression if the type of if and else cases does not match" $ do
      let code   = unlines ["if (true) {", "  \"OK\"", "}", "else {", "  1", "}"]
          actual = tester code
      snapshotTest "should fail to infer an if else expression if the type of if and else cases does not match" actual

    it "should infer a ternary expression" $ do
      let code   = unlines ["true ? \"OK\" : \"NOT OK\""]
          actual = tester code
      snapshotTest "should infer a ternary expression" actual

    ---------------------------------------------------------------------------


    -- Pattern matching:

    it "should resolve where with a Boolean literal" $ do
      let code   = unlines ["where(true) {", "  true => \"OK\"", "  false => \"NOT OK\"", "}"]
          actual = tester code
      snapshotTest "should resolve where with a Boolean literal" actual

    it "should resolve where with a Integer input" $ do
      let
        code = unlines
          ["where(42) {", "  1 => \"NOPE\"", "  3 => \"NOPE\"", "  33 => \"NOPE\"", "  42 => \"YEAH\"", "}"]
        actual = tester code
      snapshotTest "should resolve where with a Integer input" actual

    it "should resolve where with a string input" $ do
      let code =
            unlines ["where(\"42\") {", "  \"1\" => 1", "  \"3\" => 3", "  \"33\" => 33", "  \"42\" => 42", "}"]
          actual = tester code
      snapshotTest "should resolve where with a string input" actual

    it "should resolve where with an ADT that has unary constructors" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , "perhaps = Just(4)"
            , "where(perhaps) {"
            , "  Just(a) => a"
            , "  Nothing => 0"
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve where with an ADT that has unary constructors" actual

    it "should fail to resolve a pattern when the pattern constructor does not match the ADT" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , "type Failure = Nope"
            , "perhaps = Nope"
            , "where(perhaps) {"
            , "  Just(a) => a"
            , "  Nothing => 0"
            , "}"
            ]
          actual = tester code
      snapshotTest "should fail to resolve a pattern when the pattern constructor does not match the ADT" actual

    it "should fail to resolve a pattern when the pattern constructor does not match the constructor arg types" $ do
      let code = unlines
            [ "type User = LoggedIn(String, Integer)"
            , "u = LoggedIn(\"John\", 33)"
            , "where(u) {"
            , "  LoggedIn(Integer, x) => x"
            , "}"
            ]
          actual = tester code
      snapshotTest
        "should fail to resolve a pattern when the pattern constructor does not match the constructor arg types"
        actual

    it "should fail to resolve a constructor pattern with different type variables applied" $ do
      let code = unlines
            [ "type User a = LoggedIn(a, Integer)"
            , "u = LoggedIn(\"John\", 33)"
            , "where(u) {"
            , "  LoggedIn(Integer, x) => x"
            , "  LoggedIn(String, x) => x"
            , "}"
            ]
          actual = tester code
      snapshotTest "should fail to resolve a constructor pattern with different type variables applied" actual

    it "should fail to resolve if the given constructor does not exist" $ do
      let code   = unlines ["where(3) {", "  LoggedIn(Integer, x) => x", "  LoggedIn(String, x) => x", "}"]
          actual = tester code
      snapshotTest "should fail to resolve if the given constructor does not exist" actual

    it "should resolve basic patterns for lists" $ do
      let code = unlines
            [ "where([1, 2, 3, 5, 8]) {"
            , "  [1, 2, 3] => 1"
            , "  [1, 2, n] => n"
            , "  [n, 3] => n"
            , "  [x, y, z] => x + y + z"
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve basic patterns for lists" actual

    it "should fail to resolve patterns of different types for list items" $ do
      let code   = unlines ["where([1, 2, 3, 5, 8]) {", "  [1, 2, 3] => 1", "  [\"1\", n] => n", "}"]
          actual = tester code
      snapshotTest "should fail to resolve patterns of different types for list items" actual

    it "should allow deconstruction of lists" $ do
      let code   = unlines ["where([1, 2, 3, 5, 8]) {", "  [1, 2, ...rest] => rest", "}"]
          actual = tester code
      snapshotTest "should allow deconstruction of lists" actual

    it "should correctly infer types of record pattern when the input has a variable type" $ do
      let code   = unlines ["fn2 = (a) => (where(a) {", "  { z: z } => z", "  { x: x } => x", "})"]
          actual = tester code
      snapshotTest "should correctly infer types of record pattern when the input has a variable type" actual

    it "should correctly infer constructor patterns given a var" $ do
      let code = unlines
            [ "type Maybe a = Just(a) | Nothing"
            , "fn = (b) => ("
            , "  where(b) {"
            , "    Just(x) => x"
            , "  }"
            , ")"
            , "fn(Just(3))"
            ]
          actual = tester code
      snapshotTest "should correctly infer constructor patterns given a var" actual

    it "should correctly infer shorthand syntax for record property matching" $ do
      let code   = unlines ["fn = (r) => (", "  where(r) {", "    { x, y } => x + y", "  }", ")"]
          actual = tester code
      snapshotTest "should correctly infer shorthand syntax for record property matching" actual

    it "should resolve ADTs with 3 parameters in is" $ do
      let code   = unlines ["export type Wish e a c = Wish(e, a, c)", "where(Wish(1, 2, 3)) {", "  Wish(_, _, c) => c", "}"]
          actual = tester code
      snapshotTest "should resolve ADTs with 3 parameters in is" actual

    it "should fail to resolve patterns for namespaced ADTs that do not exist" $ do
      let code   = unlines ["might = (x) => where(x) {", "  M.Maybe(a) => a", "}"]
          actual = tester code
      snapshotTest "should fail to resolve patterns for namespaced ADTs that do not exist" actual

    it "should fail to resolve patterns for namespaced ADTs that do not exist when the namespace exists" $ do
      let codeA  = ""
          astA   = buildAST "./ModuleA" codeA
          codeB  = unlines ["import M from \"./ModuleA\"", "", "might = (x) => where(x) {", "  M.Maybe(a) => a", "}"]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
      snapshotTest "should fail to resolve patterns for namespaced ADTs that do not exist when the namespace exists"
                   actual

    ---------------------------------------------------------------------------


    -- Imports:

    it "should resolve names from imported modules" $ do
      let codeA  = unlines ["export inc = (a) => (a + 1)", "", "add = (a, b) => (a + b)", "addThree = add(3)"]
          astA   = buildAST "./ModuleA" codeA
          codeB  = unlines ["import { inc } from \"./ModuleA\"", "inc(3)"]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
      snapshotTest "should resolve names from imported modules" actual

    it "should resolve namespaced imports" $ do
      let codeA  = "export singleton = (a) => ([a])"
          astA   = buildAST "./ModuleA" codeA
          codeB  = unlines ["import L from \"./ModuleA\"", "L.singleton(3)"]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
      snapshotTest "should resolve namespaced imports" actual

    it "should resolve usage of exported names" $ do
      let code   = unlines ["export inc = (a) => a + 1", "inc(3)"]
          actual = tester code
      snapshotTest "should resolve usage of exported names" actual

    it "should resolve usage of exported typed names" $ do
      let code   = unlines ["inc :: Integer -> Integer", "export inc = (a) => (a + 1)", "inc(3)"]
          actual = tester code
      snapshotTest "should resolve usage of exported typed names" actual

    it "should resolve ADT typings without vars" $ do
      let codeA  = "export type Something = Something"
          astA   = buildAST "./ModuleA" codeA
          codeB  = unlines ["import S from \"./ModuleA\"", "fn :: S.Something -> S.Something", "export fn = (x) => x"]
          astB   = buildAST "./ModuleB" codeB
      let actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)] in tableTester astTable b
      snapshotTest "should resolve ADT typings without vars" actual

    ---------------------------------------------------------------------------


    -- Pipe operator:

    it "should resolve the pipe operator" $ do
      let code   = unlines ["inc = (a) => (a + 1)", "3 |> inc"]
          actual = tester code
      snapshotTest "should resolve the pipe operator" actual

    ---------------------------------------------------------------------------


    -- Boolean operators:

    it "should resolve the and operator" $ do
      let code   = "true && false"
          actual = tester code
      snapshotTest "should resolve the and operator" actual

    it "should resolve the or operator" $ do
      let code   = "true || false"
          actual = tester code
      snapshotTest "should resolve the or operator" actual

    it "should resolve the combination of and and or operators" $ do
      let code   = "true || false && true"
          actual = tester code
      snapshotTest "should resolve the combination of and and or operators" actual

    it "should resolve the gt operator" $ do
      let code   = "1 > 3"
          actual = tester code
      snapshotTest "should resolve the gt operator" actual

    it "should resolve the lt operator" $ do
      let code   = "1 < 3"
          actual = tester code
      snapshotTest "should resolve the lt operator" actual

    it "should resolve the gte operator" $ do
      let code   = "1 >= 3"
          actual = tester code
      snapshotTest "should resolve the gte operator" actual

    it "should resolve the lte operator" $ do
      let code   = "1 <= 3"
          actual = tester code
      snapshotTest "should resolve the lte operator" actual

    it "should resolve the negation operator" $ do
      let code   = "!false"
          actual = tester code
      snapshotTest "should resolve the negation operator" actual

    ---------------------------------------------------------------------------


    -- Typed expressions:

    it "should validate correct type annotations" $ do
      let code = unlines
            [ "inc :: Integer -> Integer"
            , "inc = (a) => (a + 1)"
            , "(3 :: Integer)"
            , "type Maybe a = Just(a) | Nothing"
            , "(Nothing :: Maybe a)"
            -- TODO: The surrounded parens are necessary for now as the grammar is too ambiguous.
            -- We need to split the production and reconnect it when building the canonical AST.
            , "(Just(3) :: Maybe Integer)"
            ]
          actual = tester code
      snapshotTest "should validate correct type annotations" actual

    it "should validate type annotations and instantiate their variables" $ do
      let code = unlines
            [ "map :: (a -> b) -> List a -> List b"
            , "map = (f, xs) => (#- some JS -#)"
            , "[[1, 2], [3, 4]]"
            , "  |> map(map((x) => (x * 2)))"
            ]
          actual = tester code
      snapshotTest "should validate type annotations and instantiate their variables" actual

    it "should validate type annotations for ADTs that have no param" $ do
      let code   = unlines ["type X = X", "x = (X :: X)"]
          actual = tester code
      snapshotTest "should validate type annotations for ADTs that have no param" actual

    it "should parse functions as args for adts in typings correctly" $ do
      let code = unlines
            [ "map :: (a -> b) -> List a -> List b"
            , "map = (f, xs) => (#- some JS -#)"
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
            ]
          actual = tester code
      snapshotTest "should parse functions as args for adts in typings correctly" actual

    ---------------------------------------------------------------------------



    -- Recursion:

    it "should resolve recursive functions" $ do
      let code   = "fn = (x) => (x + fn(x))"
          actual = tester code
      snapshotTest "should resolve recursive functions" actual

    it "should resolve fibonacci recursive function" $ do
      let code   = "fib = (n) => (if (n < 2) { n } else { fib(n - 1) + fib(n - 2) })"
          actual = tester code
      snapshotTest "should resolve fibonacci recursive function" actual

    ---------------------------------------------------------------------------



    -- Template Strings:

    it "should resolve template strings" $ do
      let code = unlines
            [ "x = if(true) { 'it is true' } else { 'it is false' }"
            , "`probably ${x}!`"
            , ""
            , "`3 + 7 is ${if(3 + 7 > 10) { 'more than 10' } else { 'less than 10' } }`" -- With some more complex interpolated things
            ]
          actual = tester code
      snapshotTest "should resolve template strings" actual

    it "should fail to solve template strings when interpolated expressions are not strings" $ do
      let code   = "`${4 + 3}!`"
          actual = tester code
      snapshotTest "should fail to solve template strings when interpolated expressions are not strings" actual

    -- Scope

    it "should figure out illegal recursive accesses" $ do
      let code   = unlines ["x = x + 1"]
          actual = case buildAST "./Module" code of
            Right parsed -> tableTester (M.fromList [("./Module", parsed)]) parsed
            Left  e      -> Left e
      snapshotTest "should figure out illegal recursive accesses" actual

    it "should figure out illegal recursive accesses within function bodies" $ do
      let code   = unlines ["g = (x) => {", "  p = p + 1", "", "  return p", "}"]
          actual = case buildAST "./Module" code of
            Right parsed -> tableTester (M.fromList [("./Module", parsed)]) parsed
            Left  e      -> Left e
      snapshotTest "should figure out illegal recursive accesses within function bodies" actual

    it "should fail when accessing a function without typing that is defined after" $ do
      let code   = unlines ["f = (x) => definedAfter(x)", "definedAfter = (x) => x + 1"]
          actual = case buildAST "./Module" code of
            Right parsed -> tableTester (M.fromList [("./Module", parsed)]) parsed
            Left  e      -> Left e
      snapshotTest "should fail when accessing a function without typing that is defined after" actual

    it "should fail when accessing a executing an expression declared later" $ do
      let code   = unlines ["definedAfter(3)", "definedAfter :: Integer -> Integer", "definedAfter = (x) => x + 1"]
          actual = case buildAST "./Module" code of
            Right parsed -> tableTester (M.fromList [("./Module", parsed)]) parsed
            Left  e      -> Left e
      snapshotTest "should fail when accessing a executing an expression declared later" actual

    it "should fail when shadowing a name even if it's defined after the function" $ do
      let code   = unlines ["g = (x) => {", "  a = 2", "  return a", "}", "a = 4"]
          actual = case buildAST "./Module" code of
            Right parsed -> tableTester (M.fromList [("./Module", parsed)]) parsed
            Left  e      -> Left e
      snapshotTest "should fail when shadowing a name even if it's defined after the function" actual

    it "should fail when exporting a name not defined yet" $ do
      let code   = unlines ["export definedAfter", "definedAfter :: Integer -> Integer", "definedAfter = (x) => x + 1"]
          actual = case buildAST "./Module" code of
            Right parsed -> tableTester (M.fromList [("./Module", parsed)]) parsed
            Left  e      -> Left e
      snapshotTest "should fail when exporting a name not defined yet" actual
