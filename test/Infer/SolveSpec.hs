module Infer.SolveSpec where

import qualified Data.Map                      as M
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
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
import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Error.Error
import           Explain.Reason
import           AST

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

-- TODO: Refactor in order to use the inferAST function instead that supports imports
tester :: String -> Either InferError Slv.AST
tester code = case buildAST "path" code of
  (Right ast) -> runEnv ast >>= (`runInfer` ast)
  _           -> Left $ InferError (UnboundVariable "") NoReason
 where
  runEnv x =
    fst <$> runExcept (runStateT (buildInitialEnv x) Unique { count = 0 })

tableTester :: Src.Table -> Src.AST -> Either InferError Slv.Table
tableTester table ast =
  fst <$> runExcept (runStateT (inferAST "./" table ast) Unique { count = 0 })

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

    it "should infer tripleEq operator" $ do
      let code   = "1 == 3"
          actual = tester code
      snapshotTest "should infer tripleEq operator" actual

    it "should infer wrapped douleEq operator" $ do
      let code   = "((a, b) => (a == b))(1, 3)"
          actual = tester code
      snapshotTest "should infer wrapped douleEq operator" actual

    it "should infer asList function" $ do
      let code   = "(a) => (asList(a))"
          actual = tester code
      snapshotTest "should infer asList function" actual

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

    it
        "should return an error when an ADT defines a constructor with an unbound variable"
      $ do
          let code   = unlines ["data Result a = Success b", ""]
              actual = tester code
          snapshotTest
            "should return an error when an ADT defines a constructor with an unbound variable"
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

    ---------------------------------------------------------------------------


    -- Records:

    it "should infer a record field access" $ do
      let code   = unlines ["a = { x: 3, y: 5 }", "a.x"]
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
      let code   = "({ x: 3, y: 7 } :: { x :: Num, y :: Num })"
          actual = tester code
      snapshotTest "should infer a record with a type annotation" actual

    it "should infer abstraction param that is a deep record" $ do
      let code   = "f = (x) => (x.a.b.c.d.e)"
          actual = tester code
      snapshotTest "should infer abstraction param that is a deep record" actual

    it "should infer abstraction param that having a record exp as body" $ do
      let code   = "addTodo = (state) => ({ ...state, x: \"3\", y: state.y })"
          actual = tester code
      snapshotTest
        "should infer abstraction param that having a record exp as body"
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

    it "should infer fail when spreading an array of a different type" $ do
      let code   = unlines ["[ 1, ...[\"1\", \"2\"]]"]
          actual = tester code
      snapshotTest
        "should infer fail when spreading an array of a different type"
        actual

    ---------------------------------------------------------------------------


    -- Applications:

    it "should fail for applications with a wrong argument type" $ do
      let code   = unlines ["fn = (a, b) => (a == b)", "fn(\"3\", 4)"]
          actual = tester code
      snapshotTest "should fail for applications with a wrong argument type"
                   actual

    ---------------------------------------------------------------------------


    -- Abstractions:

    -- TODO: Write and implement the same test with ADTs
    -- TODO: Write tests where implementation and definition don't match to force
    -- implementing it as it's currently not implemented.
    it "should resolve abstractions with a type definition" $ do
      let code =
            unlines
              [ "fn :: Num -> Num -> Bool"
              , "fn = (a, b) => (a == b)"
              , "fn(3, 4)"
              ]
          actual = tester code
      snapshotTest "should resolve abstractions with a type definition" actual

    it "should fail for abstractions with a wrong type definition" $ do
      let code =
            unlines
              [ "fn :: String -> Num -> Bool"
              , "fn = (a, b) => (a == b)"
              , "fn(3, 4)"
              ]
          actual = tester code
      snapshotTest "should fail for abstractions with a wrong type definition"
                   actual

    ---------------------------------------------------------------------------


    -- If/Else:

    it "should infer a simple if else expression" $ do
      let
        code = unlines
          ["if (true) {", "  \"OK\"", "}", "else {", "  \"NOT OK\"", "}"]
        actual = tester code
      snapshotTest "should infer a simple if else expression" actual

    it
        "should fail to infer an if else expression if the condition is not a Bool"
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
            "should fail to infer an if else expression if the condition is not a Bool"
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

    it "should resolve where with a boolean literal" $ do
      let code =
            unlines
              [ "where(true) {"
              , "  is true : \"OK\""
              , "  is false: \"NOT OK\""
              , "}"
              ]
          actual = tester code
      snapshotTest "should resolve where with a boolean literal" actual

    it "should resolve where with a number input" $ do
      let code = unlines
            [ "where(42) {"
            , "  case 1 : \"NOPE\""
            , "  case 3 : \"NOPE\""
            , "  case 33: \"NOPE\""
            , "  case 42: \"YEAH\""
            , "}"
            ]
          actual = tester code
      snapshotTest "should resolve where with a number input" actual

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
            , "  is Just Num   : 2"
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
                [ "data User = LoggedIn String Num"
                , "u = LoggedIn(\"John\", 33)"
                , "where(u) {"
                , "  is LoggedIn Num x: x"
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
                [ "data User a = LoggedIn a Num"
                , "u = LoggedIn(\"John\", 33)"
                , "where(u) {"
                , "  is LoggedIn Num x   : x"
                , "  is LoggedIn String x: x"
                , "}"
                ]
              actual = tester code
          snapshotTest
            "should fail to resolve a constructor pattern with different type variables applied"
            actual

    it "should fail to resolve if the given constructor does not exist" $ do
      let code = unlines
            [ "where(3) {"
            , "  is LoggedIn Num x   : x"
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
                [ "fn2 = (x) => (where(x) {"
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

    -- TODO: Add tests with bigger constructors ( 2, 3, 4, 5 -aries ) and update
    -- implementation to get out of the that weird handling in generateCaseEnv

    ---------------------------------------------------------------------------


    -- Imports:

    it "should resolve names from imported modules" $ do
      let codeA  = "export inc = (a) => (a + 1)"
          astA   = buildAST "./ModuleA.mad" codeA
          codeB  = unlines ["import { inc } from \"ModuleA\"", "inc(3)"]
          astB   = buildAST "./ModuleB.mad" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable =
                      M.fromList [("./ModuleA.mad", a), ("./ModuleB.mad", b)]
              in  tableTester astTable b
      snapshotTest "should resolve names from imported modules" actual

    it "should resolve namespaced imports" $ do
      let codeA  = "export singleton = (a) => ([a])"
          astA   = buildAST "./ModuleA.mad" codeA
          codeB  = unlines ["import L from \"ModuleA\"", "L.singleton(3)"]
          astB   = buildAST "./ModuleB.mad" codeB
          actual = case (astA, astB) of
            (Right a, Right b) ->
              let astTable =
                      M.fromList [("./ModuleA.mad", a), ("./ModuleB.mad", b)]
              in  tableTester astTable b
      snapshotTest "should resolve namespaced imports" actual

    it "should resolve usage of exported names" $ do
      let code   = unlines ["export inc = (a) => (a + 1)", "inc(3)"]
          actual = tester code
      snapshotTest "should resolve usage of exported names" actual

    it "should resolve usage of exported typed names" $ do
      let
        code =
          unlines ["inc :: Num -> Num", "export inc = (a) => (a + 1)", "inc(3)"]
        actual = tester code
      snapshotTest "should resolve usage of exported typed names" actual

    ---------------------------------------------------------------------------


    -- Pipe operator:

    it "should resolve the pipe operator" $ do
      let code   = unlines ["inc = (a) => (a + 1)", "3 |> inc"]
          actual = tester code
      snapshotTest "should resolve the pipe operator" actual

    ---------------------------------------------------------------------------


    -- Bool operators:

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
            [ "inc :: Num -> Num"
            , "inc = (a) => (a + 1)"
            , "(3 :: Num)"
            , "data Maybe a = Just a | Nothing"
            , "(Nothing :: Maybe a)"
            -- TODO: The surrounded parens are necessary for now as the grammar is too ambiguous.
            -- We need to split the production and reconnect it when building the canonical AST.
            , "(Just(3) :: Maybe Num)"
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
