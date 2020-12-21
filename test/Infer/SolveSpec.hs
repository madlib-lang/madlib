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
import           Infer.Solve
import           Infer.Env
import           Infer.Infer
import           Error.Error
import           Explain.Reason
import           Parse.AST

snapshotTest :: Show a => String -> a -> Golden Text
snapshotTest name actualOutput = Golden
  { output        = pack $ ppShow actualOutput
  , encodePretty  = ppShow
  , writeToFile   = T.writeFile
  , readFromFile  = T.readFile
  , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
  , directory     = ".snapshots"
  , failFirstTime = False
  }

tester :: String -> Either InferError Slv.AST
tester code = case buildAST "path" code of
  (Right ast) -> runEnv ast >>= (`runInfer` ast)
  (Left  e  ) -> Left e
 where
  runEnv x = fst <$> runExcept
    (runStateT (buildInitialEnv initialEnv x) Unique { count = 0 })

tableTester :: Src.Table -> Src.AST -> Either InferError Slv.Table
tableTester table ast =
  fst <$> runExcept (runStateT (solveTable table ast) Unique { count = 0 })

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
      let code = unlines
            [ "data Result = Success String | Error"
            , "result = Success(\"response\")"
            ]
          actual = tester code
      snapshotTest "should infer adts" actual

    it "should infer adts with type parameters" $ do
      let code = unlines
            [ "data Result a"
            , "  = Success a"
            , "  | Error"
            , "result = Success(true)"
            ]
          actual = tester code
      snapshotTest "should infer adts with type parameters" actual

    it "should infer application of adts" $ do
      let code = unlines
            [ "data Result = Success String | Error"
            , "result1 = Success(\"response\")"
            , "result2 = Error"
            , "((a, b) => (a == b))(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer application of adts" actual

    it "should infer adt return for abstractions" $ do
      let code = unlines
            [ "data Result a = Success a | Error"
            , "result1 = Success(\"response\")"
            , "result2 = Error"
            , "((a, b) => (a == b))(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer adt return for abstractions" actual

    it "should return an error when an ADT defines a type already existing" $ do
      let code = unlines
            [ "data Result a = Success a | Error"
            , "data Result a = Success a | Error"
            ]
          actual = tester code
      snapshotTest
        "should return an error when an ADT defines a type already existing"
        actual

    it "should accept ADTs with constructors that have free vars" $ do
      let code   = unlines ["data Result = Success b", ""]
          actual = tester code
      snapshotTest "should accept ADTs with constructors that have free vars"
                   actual

    it "should infer adts with record constructors" $ do
      let
        code = unlines
          [ "data Result = Success { value :: String } | Error { message :: String }"
          , "result1 = Success({ value: `42` })"
          , "result2 = Error({ message: 'Err' })"
          , "((a, b) => (a == b))(result1, result2)"
          ]
        actual = tester code
      snapshotTest "should infer adts with record constructors" actual

    it "should infer params for adts" $ do
      let code = unlines
            [ "data Result = Success { value :: String }"
            , "r = Success { value: \"42\" }"
            , "((a) => (a))(r)"
            ]
          actual = tester code
      snapshotTest "should infer params for adts" actual

    it "should fail if it uses an ADT not defined" $ do
      let code = unlines
            [ "addNegativeTen :: Maybe Number -> Number"
            , "export addNegativeTen = (a) => (a + -10)"
            , "addNegativeTen(3)"
            ]
          actual = tester code
      snapshotTest "should fail if it uses an ADT not defined" actual

    it "should fail if it uses an ADT not defined in patterns" $ do
      let code   = unlines ["where(3) {", "  is NotExisting: 5", "}"]
          actual = tester code
      snapshotTest "should fail if it uses an ADT not defined in patterns"
                   actual

    it "should resolve ADTs with function parameters" $ do
      let code = unlines
            [ "export data Wish e a = Wish ((e -> m) -> (a -> m) -> m)"
            , "Wish((bad, good) => (good(3)))"
            ]
          actual = tester code
      snapshotTest "should resolve ADTs with function parameters" actual

    it "should resolve namespaced ADTs in patterns" $ do
      let codeA = "export data Maybe a = Just a | Nothing"
          astA  = buildAST "./ModuleA" codeA
          codeB = unlines
            [ "import M from \"./ModuleA\""
            , "fn :: M.Maybe Number -> Number"
            , "export fn = (x) => ("
            , "  where(x) {"
            , "    is M.Just a : a"
            , "    is M.Nothing: -3"
            , "  }"
            , ")"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
            (Left e, _     ) -> Left e
            (_     , Left e) -> Left e
      snapshotTest "should resolve namespaced ADTs in patterns" actual

    it "should resolve namespaced ADTs in other ADTs" $ do
      let codeA = "export data Maybe a = Just a | Nothing"
          astA  = buildAST "./ModuleA" codeA
          codeB = unlines
            [ "import M from \"./ModuleA\""
            , "data MyType = MyType (M.Maybe String)"
            , "x = MyType(M.Just(\"3\"))"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
            (Left e, _     ) -> Left e
            (_     , Left e) -> Left e
      snapshotTest "should resolve namespaced ADTs in other ADTs" actual

    it "should allow ADT constructors to have record parameters" $ do
      let codeA = "export data Point = Point <Number, Number>"
          astA  = buildAST "./ModuleA" codeA
          codeB = unlines
            [ "import P from \"./ModuleA\""
            , "p = P.Point(<2, 4>)"
            , "where(p) {"
            , "  is P.Point <a, b>: a + b"
            , "}"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
            (Left e, _     ) -> Left e
            (_     , Left e) -> Left e
      snapshotTest "should allow ADT constructors to have record parameters"
                   actual

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
          astA  = buildAST "./ModuleA" codeA
          codeB = unlines
            [ "import W from \"./ModuleA\""
            , "of(3)"
            , "  |> map((x) => (x + 3))"
            , "  |> chain((x) => (of(x * 3)))"
            , "  |> W.fulfill((a) => (()), (a) => (()))"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
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
            , "instance Read Number {"
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
            , "instance Read Number {"
            , "  read = (s) => (#- parseFloat(s, 10) -#)"
            , "}"
            , ""
            , "(read('3') :: Number)"
            ]
          actual = tester code
      snapshotTest "should resolve ambiguity with type annotations" actual

    it "should resolve constrained instances" $ do
      let
        code = unlines
          [ "interface Show a {"
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
          ]
        actual = tester code
      snapshotTest "should resolve constrained instances" actual

    it "should fail for instances missing constrains" $ do
      let
        code = unlines
          [ "interface Show a {"
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
          , "instance Show <a, b> {"
          , "  show = where is <a, b>: '<' ++ show(a) ++ ', ' ++ show(b) ++ '>'"
          , "}"
          , ""
          , "show(<1, false>)"
          ]
        actual = tester code
      snapshotTest "should fail for instances missing constrains" actual



    ---------------------------------------------------------------------------



    -- Records:

    it "should infer a record field access" $ do
      let code   = unlines ["a = { x: 3.1415, y: -500 }", "a.x"]
          actual = tester code
      snapshotTest "should infer a record field access" actual

    it "should infer an App with a record" $ do
      let code = unlines
            ["a = { x: 3, y: 5 }", "xPlusY = (r) => (r.x + r.y)", "xPlusY(a)"]
          actual = tester code
      snapshotTest "should infer an App with a record" actual

    it "should fail to infer record if their fields do not match" $ do
      let code   = "{ x: 3, y: 5 } == { name: \"John\" }"
          actual = tester code
      snapshotTest "should fail to infer record if their fields do not match"
                   actual

    it "should infer a record with a type annotation" $ do
      let code   = "({ x: 3, y: 7 } :: { x :: Number, y :: Number })"
          actual = tester code
      snapshotTest "should infer a record with a type annotation" actual

    it "should infer abstraction param that is a deep record" $ do
      let code   = "f = (x) => (x.a.b.c.d.e)"
          actual = tester code
      snapshotTest "should infer abstraction param that is a deep record" actual

    it "should infer abstraction param that have a record exp as body" $ do
      let code   = "addTodo = (state) => ({ ...state, x: \"3\", y: state.y })"
          actual = tester code
      snapshotTest
        "should infer abstraction param that have a record exp as body"
        actual

    it "should fail when spreading a non spreadable type into a record" $ do
      let code   = unlines ["{ x: 1, ...3 }"]
          actual = tester code
      snapshotTest
        "should fail when spreading a non spreadable type into a record"
        actual

    it "should infer record params that are partially used in abstractions" $ do
      let code = unlines
            [ "data Maybe a = Just a | Nothing"
            , "find :: (a -> Boolean) -> List a -> Maybe a"
            , "export find = (predicate, xs) => (#- {"
            , "  const found = xs.find(predicate);"
            , "  if (found === undefined) {"
            , "    return Nothing()"
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
            , "  itemId :: Number,"
            , "  multiplier :: Number"
            , "}"
            , "alias ShopItem = {"
            , "  id :: Number,"
            , "  description :: String,"
            , "  cost :: Number,"
            , "  discounts :: List ShopDiscount"
            , "}"
            , "alias ShopCustomer = {"
            , "  id :: String,"
            , "  cart :: List ShopItem,"
            , "  money :: Number"
            , "}"
            , "alias ShopContext = {"
            , "  customers :: List ShopCustomer,"
            , "  inventory :: List ShopItem,"
            , "  sales :: List ShopDiscount"
            , "}"
            -- , ""
            -- , "buySomethingFromShop :: ShopContext -> Number -> Number -> List (Maybe String)"
            -- , "buySomethingFromShop = (ctx, itemId, customerId) => {"
            -- , "  item = find(propEq(itemId, 'id'), ctx.inventory)"
            -- , "  customer = find(propEq(customerId, 'id'), ctx.customers)"
            -- , "  return [item, customer]"
            -- , "}"
            ]
          actual = tester code
      snapshotTest
        "should infer record params that are partially used in abstractions"
        actual

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
      snapshotTest "should fail when spreading an array of a different type"
                   actual

    it "should fail when spreading a non spreadable type into an array" $ do
      let code   = unlines ["[1, ...3]"]
          actual = tester code
      snapshotTest
        "should fail when spreading a non spreadable type into an array"
        actual

    -- Tuples

    it "should infer tuple constructors" $ do
      let code   = unlines ["<1, 2, 3>", "<true, \"John\", 33>"]
          actual = tester code
      snapshotTest "should infer tuple constructors" actual

    it "should not confuse > operator with end of tuples" $ do
      let code = unlines
            [ "<1, 2, 3>"
            , "<true, \"John\", 33>"
            , "<true, \"John\", 33> 3>"
            , "<true, \"John\", \"OK\"> \"NOT OK\">"
            ]
          actual = tester code
      snapshotTest "should not confuse > operator with end of tuples" actual

    ---------------------------------------------------------------------------


    -- Applications:

    it "should fail for applications with a wrong argument type" $ do
      let code   = unlines ["fn = (a, b) => (a == b)", "fn(\"3\", 4)"]
          actual = tester code
      snapshotTest "should fail for applications with a wrong argument type"
                   actual

    it
        "should infer applications where the abstraction results from an application"
      $ do
          let
            code = unlines
              [ "data Maybe a = Just a | Nothing"
              , "flip :: (a -> b -> c) -> (b -> a -> c)"
              , "export flip = (fn) => ("
              , "  (b, a) => (fn(a, b))"
              , ")"
              , ""
              , "nth :: Number -> List a -> Maybe a"
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
          snapshotTest
            "should infer applications where the abstraction results from an application"
            actual

    ---------------------------------------------------------------------------


    -- Abstractions:

    -- TODO: Write and implement the same test with ADTs
    -- TODO: Write tests where implementation and definition don't match to force
    -- implementing it as it's currently not implemented.
    it "should resolve abstractions with a type definition" $ do
      let code = unlines
            [ "fn :: Number -> Number -> Boolean"
            , "fn = (a, b) => (a == b)"
            , "fn(3, 4)"
            ]
          actual = tester code
      snapshotTest "should resolve abstractions with a type definition" actual

    it "should fail for abstractions with a wrong type definition" $ do
      let code = unlines
            [ "fn :: String -> Number -> Boolean"
            , "fn = (a, b) => (a == b)"
            , "fn(3, 4)"
            ]
          actual = tester code
      snapshotTest "should fail for abstractions with a wrong type definition"
                   actual

    it "should resolve abstractions with many expressions" $ do
      let code = unlines
            [ "fn :: Number -> Number -> Boolean"
            , "fn = (a, b) => {"
            , "  sum = a + b"
            , "  moreThanTen = sum > 10"
            , "  computed = moreThanTen ? sum * 2 : sum / 2"
            , "  return computed % 2 == 0"
            , "}"
            , "fn(3, 4)"
            ]
          actual = tester code
      snapshotTest "should resolve abstractions with many expressions" actual

    ---------------------------------------------------------------------------


    -- If/Else:

    it "should infer a simple if else expression" $ do
      let
        code = unlines
          ["if (true) {", "  \"OK\"", "}", "else {", "  \"NOT OK\"", "}"]
        actual = tester code
      snapshotTest "should infer a simple if else expression" actual

    it
        "should fail to infer an if else expression if the condition is not a Boolean"
      $ do
          let code =
                unlines
                  [ "if (\"true\") {"
                  , "  \"OK\""
                  , "}"
                  , "else {"
                  , "  \"NOT OK\""
                  , "}"
                  ]
              actual = tester code
          snapshotTest
            "should fail to infer an if else expression if the condition is not a Boolean"
            actual

    it
        "should fail to infer an if else expression if the type of if and else cases does not match"
      $ do
          let code =
                unlines ["if (true) {", "  \"OK\"", "}", "else {", "  1", "}"]
              actual = tester code
          snapshotTest
            "should fail to infer an if else expression if the type of if and else cases does not match"
            actual

    it "should infer a ternary expression" $ do
      let code   = unlines ["true ? \"OK\" : \"NOT OK\""]
          actual = tester code
      snapshotTest "should infer a ternary expression" actual

    ---------------------------------------------------------------------------


    -- Pattern matching:

    it "should resolve where with a Boolean literal" $ do
      let code =
            unlines
              [ "where(true) {"
              , "  is true : \"OK\""
              , "  is false: \"NOT OK\""
              , "}"
              ]
          actual = tester code
      snapshotTest "should resolve where with a Boolean literal" actual

    it "should resolve where with a Number input" $ do
      let code = unlines
            [ "where(42) {"
            , "  is 1 : \"NOPE\""
            , "  is 3 : \"NOPE\""
            , "  is 33: \"NOPE\""
            , "  is 42: \"YEAH\""
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve where with a Number input" actual

    it "should resolve where with a string input" $ do
      let code = unlines
            [ "where(\"42\") {"
            , "  is \"1\" : 1"
            , "  is \"3\" : 3"
            , "  is \"33\": 33"
            , "  is \"42\": 42"
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve where with a string input" actual

    it "should resolve where with constant type constructor is cases" $ do
      let code   = unlines ["where(\"42\") {", "  is String : 1", "}"]
          actual = tester code
      snapshotTest
        "should resolve where with constant type constructor is cases"
        actual

    it "should resolve where with an ADT that has unary constructors" $ do
      let code = unlines
            [ "data Maybe a = Just a | Nothing"
            , "perhaps = Just(4)"
            , "where(perhaps) {"
            , "  is Just a: a"
            , "  is Nothing: 0"
            , "}"
            ]
          actual = tester code
      snapshotTest
        "should resolve where with an ADT that has unary constructors"
        actual

    it "should resolve where with an ADT and PCon patterns" $ do
      let code = unlines
            [ "data Maybe a = Just a | Nothing"
            , "perhaps = Just(4)"
            , "where(perhaps) {"
            , "  is Just Number   : 2"
            , "  is Nothing    : 0"
            , "  is Just _     : 1"
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve where with an ADT and PCon patterns" actual

    it
        "should fail to resolve a pattern when the pattern constructor does not match the ADT"
      $ do
          let code = unlines
                [ "data Maybe a = Just a | Nothing"
                , "data Failure = Nope"
                , "perhaps = Nope"
                , "where(perhaps) {"
                , "  is Just a: a"
                , "  is Nothing: 0"
                , "}"
                ]
              actual = tester code
          snapshotTest
            "should fail to resolve a pattern when the pattern constructor does not match the ADT"
            actual

    it
        "should fail to resolve a pattern when the pattern constructor does not match the constructor arg types"
      $ do
          let code = unlines
                [ "data User = LoggedIn String Number"
                , "u = LoggedIn(\"John\", 33)"
                , "where(u) {"
                , "  is LoggedIn Number x: x"
                , "}"
                ]
              actual = tester code
          snapshotTest
            "should fail to resolve a pattern when the pattern constructor does not match the constructor arg types"
            actual

    it
        "should fail to resolve a constructor pattern with different type variables applied"
      $ do
          let code = unlines
                [ "data User a = LoggedIn a Number"
                , "u = LoggedIn(\"John\", 33)"
                , "where(u) {"
                , "  is LoggedIn Number x   : x"
                , "  is LoggedIn String x   : x"
                , "}"
                ]
              actual = tester code
          snapshotTest
            "should fail to resolve a constructor pattern with different type variables applied"
            actual

    it "should fail to resolve if the given constructor does not exist" $ do
      let code = unlines
            [ "where(3) {"
            , "  is LoggedIn Number x   : x"
            , "  is LoggedIn String x: x"
            , "}"
            ]
          actual = tester code
      snapshotTest
        "should fail to resolve if the given constructor does not exist"
        actual

    it "should resolve basic patterns for lists" $ do
      let code = unlines
            [ "where([1, 2, 3, 5, 8]) {"
            , "  is [1, 2, 3]: 1"
            , "  is [1, 2, n]: n"
            , "  is [n, 3]   : n"
            , "  is [x, y, z]: x + y + z"
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve basic patterns for lists" actual

    it "should fail to resolve patterns of different types for list items" $ do
      let code = unlines
            [ "where([1, 2, 3, 5, 8]) {"
            , "  is [1, 2, 3] : 1"
            , "  is [\"1\", n]: n"
            , "}"
            ]
          actual = tester code
      snapshotTest
        "should fail to resolve patterns of different types for list items"
        actual

    it "should allow deconstruction of lists" $ do
      let
        code = unlines
          ["where([1, 2, 3, 5, 8]) {", "  is [1, 2, ...rest]: rest", "}"]
        actual = tester code
      snapshotTest "should allow deconstruction of lists" actual

    it "should allow deconstruction of records" $ do
      let code =
            unlines
              [ "where({ x: 1, y: 2, z: 3 }) {"
              , "  is { x: 1, ...rest }: rest.z"
              , "}"
              ]
          actual = tester code
      snapshotTest "should allow deconstruction of records" actual

    it
        "should correctly infer types of record pattern when the input has a variable type"
      $ do
          let code = unlines
                [ "fn2 = (a) => (where(a) {"
                , "  is { z: z }: z"
                , "  is { x: x }: x"
                , "})"
                ]
              actual = tester code
          snapshotTest
            "should correctly infer types of record pattern when the input has a variable type"
            actual

    it "should correctly infer types of spread record patterns" $ do
      let
        code = unlines
          [ "where({ x: 4, name: \"John\", job: \"Accountant\", fulfilled: false }) {"
          , "  is { name: name }: name"
          , "  is { x: x, ...b }: b.name"
          , "}"
          ]
        actual = tester code
      snapshotTest "should correctly infer types of spread record patterns"
                   actual

    it "should correctly infer fields accessed through spread pattern" $ do
      let code = unlines
            [ "fn = (a) => (where(a) {"
            , "  is { x: x, ...b }: b.z"
            , "  is { x: x }: x"
            , "})"
            ]
          actual = tester code
      snapshotTest
        "should correctly infer fields accessed through spread pattern"
        actual

    it "should correctly infer constructor patterns given a var" $ do
      let code = unlines
            [ "data Maybe a = Just a | Nothing"
            , "fn = (b) => ("
            , "  where(b) {"
            , "    is Just x: x"
            , "  }"
            , ")"
            , "fn(Just(3))"
            ]
          actual = tester code
      snapshotTest "should correctly infer constructor patterns given a var"
                   actual

    it "should correctly infer nested spread patterns" $ do
      let code = unlines
            [ "fn = (r) => ("
            , "  where(r) {"
            , "    is { x: { y: { y: y }, ...k }, ...c }: y + k.z + c.o + c.i"
            , "  }"
            , ")"
            ]
          actual = tester code
      snapshotTest "should correctly infer nested spread patterns" actual

    it "should correctly infer shorthand syntax for record property matching"
      $ do
          let code =
                unlines
                  [ "fn = (r) => ("
                  , "  where(r) {"
                  , "    is { x, y }: x + y"
                  , "  }"
                  , ")"
                  ]
              actual = tester code
          snapshotTest
            "should correctly infer shorthand syntax for record property matching"
            actual

    it "should resolve ADTs with 3 parameters in is" $ do
      let code = unlines
            [ "export data Wish e a c = Wish e a c"
            , "where(Wish(1, 2, 3)) {"
            , "  is Wish _ _ c: c"
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve ADTs with 3 parameters in is" actual

    it "should fail to resolve patterns for namespaced ADTs that do not exist"
      $ do
          let
            code =
              unlines ["might = (x) => (where(x) {", "  is M.Maybe a: a", "})"]
            actual = tester code
          snapshotTest
            "should fail to resolve patterns for namespaced ADTs that do not exist"
            actual

    it
        "should fail to resolve patterns for namespaced ADTs that do not exist when the namespace exists"
      $ do
          let codeA = ""
              astA  = buildAST "./ModuleA" codeA
              codeB = unlines
                [ "import M from \"./ModuleA\""
                , ""
                , "might = (x) => (where(x) {"
                , "  is M.Maybe a: a"
                , "})"
                ]
              astB   = buildAST "./ModuleB" codeB
              actual = case (astA, astB) of
                (Right a, Right b) ->
                  let astTable =
                          M.fromList [("./ModuleA", a), ("./ModuleB", b)]
                  in  tableTester astTable b
          snapshotTest
            "should fail to resolve patterns for namespaced ADTs that do not exist when the namespace exists"
            actual

    ---------------------------------------------------------------------------


    -- Imports:

    it "should resolve names from imported modules" $ do
      let codeA = unlines
            [ "export inc = (a) => (a + 1)"
            , ""
            , "add = (a, b) => (a + b)"
            , "addThree = add(3)"
            ]
          astA   = buildAST "./ModuleA" codeA
          codeB  = unlines ["import { inc } from \"./ModuleA\"", "inc(3)"]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
      snapshotTest "should resolve names from imported modules" actual

    it "should resolve namespaced imports" $ do
      let codeA  = "export singleton = (a) => ([a])"
          astA   = buildAST "./ModuleA" codeA
          codeB  = unlines ["import L from \"./ModuleA\"", "L.singleton(3)"]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
      snapshotTest "should resolve namespaced imports" actual

    it "should resolve usage of exported names" $ do
      let code   = unlines ["export inc = (a) => (a + 1)", "inc(3)"]
          actual = tester code
      snapshotTest "should resolve usage of exported names" actual

    it "should resolve usage of exported typed names" $ do
      let code =
            unlines
              [ "inc :: Number -> Number"
              , "export inc = (a) => (a + 1)"
              , "inc(3)"
              ]
          actual = tester code
      snapshotTest "should resolve usage of exported typed names" actual

    it "should resolve ADT typings without vars" $ do
      let codeA = "export data Something = Something"
          astA  = buildAST "./ModuleA" codeA
          codeB = unlines
            [ "import S from \"./ModuleA\""
            , "fn :: S.Something -> S.Something"
            , "export fn = (x) => (x)"
            ]
          astB   = buildAST "./ModuleB" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable = M.fromList [("./ModuleA", a), ("./ModuleB", b)]
              in  tableTester astTable b
      snapshotTest "should resolve ADT typings without vars" actual

    ---------------------------------------------------------------------------


    -- Pipe operator:

    it "should resolve the pipe operator" $ do
      let code   = unlines ["inc = (a) => (a + 1)", "3 |> inc"]
          actual = tester code
      snapshotTest "should resolve the pipe operator" actual

    ---------------------------------------------------------------------------


    -- Boolean operators:

    it "should resolve the operator &&" $ do
      let code   = "true && false"
          actual = tester code
      snapshotTest "should resolve the operator &&" actual

    it "should resolve the operator ||" $ do
      let code   = "true || false"
          actual = tester code
      snapshotTest "should resolve the operator ||" actual

    it "should resolve the combination of && and ||" $ do
      let code   = "true || false && true"
          actual = tester code
      snapshotTest "should resolve the combination of && and ||" actual

    it "should resolve the operator >" $ do
      let code   = "1 > 3"
          actual = tester code
      snapshotTest "should resolve the operator >" actual

    it "should resolve the operator <" $ do
      let code   = "1 < 3"
          actual = tester code
      snapshotTest "should resolve the operator <" actual

    it "should resolve the operator >=" $ do
      let code   = "1 >= 3"
          actual = tester code
      snapshotTest "should resolve the operator >=" actual

    it "should resolve the operator <=" $ do
      let code   = "1 <= 3"
          actual = tester code
      snapshotTest "should resolve the operator <=" actual

    it "should resolve the operator !" $ do
      let code   = "!false"
          actual = tester code
      snapshotTest "should resolve the operator !" actual

    ---------------------------------------------------------------------------


    -- Typed expressions:

    it "should validate correct type annotations" $ do
      let code = unlines
            [ "inc :: Number -> Number"
            , "inc = (a) => (a + 1)"
            , "(3 :: Number)"
            , "data Maybe a = Just a | Nothing"
            , "(Nothing :: Maybe a)"
            -- TODO: The surrounded parens are necessary for now as the grammar is too ambiguous.
            -- We need to split the production and reconnect it when building the canonical AST.
            , "(Just(3) :: Maybe Number)"
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
      snapshotTest
        "should validate type annotations and instantiate their variables"
        actual

    it "should validate type annotations for ADTs that have no param" $ do
      let code   = unlines ["data X = X", "x = (X :: X)"]
          actual = tester code
      snapshotTest
        "should validate type annotations for ADTs that have no param"
        actual

    it "should parse functions as args for adts in typings correctly" $ do
      let
        code = unlines
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
      snapshotTest
        "should parse functions as args for adts in typings correctly"
        actual



    ---------------------------------------------------------------------------


    -- Recursion:

    it "should resolve recursive functions" $ do
      let code   = "fn = (x) => (x + fn(x))"
          actual = tester code
      snapshotTest "should resolve recursive functions" actual

    it "should resolve fibonacci recursive function" $ do
      let code =
            "fib = (n) => (if (n < 2) { n } else { fib(n - 1) + fib(n - 2) })"
          actual = tester code
      snapshotTest "should resolve fibonacci recursive function" actual
