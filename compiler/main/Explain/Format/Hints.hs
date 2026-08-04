{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Explain.Format.Hints
  ( noInstanceSmartHints
  , operatorHints
  , grammarSmartHints
  , stdlibMap
  , mkUnificationTitle
  , typeMismatchTitle
  , describeType
  , inlineOrDescribe
  , arityMismatch
  , toOrdinal
  ) where

import           Error.Error (ErrorOrigin(..), BranchSide(..))
import           Infer.Type
import           Explain.Diagnostic             ( Note
                                                , hint
                                                , note
                                                )
import           Explain.Format.TypeDiff        ( firstDifference
                                                , isTuple
                                                , prettyPrintType
                                                , toOrdinal
                                                )
import qualified Data.List as List
import qualified Data.Map as M


-- | Generates a descriptive title for unification errors based on the origin context.
mkUnificationTitle :: Type -> Type -> ErrorOrigin -> String
mkUnificationTitle found expected origin =
  case origin of
    FromOperator "+" ->
      case (found, expected) of
        (TCon (TC "String" _) _ _, _) -> "Cannot use '+' with String — did you mean '<>'?"
        (_, TCon (TC "String" _) _ _) -> "Cannot use '+' with String — did you mean '<>'?"
        _ -> "Operands of '+' must have the same type"
    FromOperator "&&" ->
      "Both sides of '&&' must be Boolean"
    FromOperator "||" ->
      "Both sides of '||' must be Boolean"
    FromOperator "<>" ->
      "Both sides of '<>' must have the same type"
    FromOperator "++" ->
      "Both sides of '++' must be the same List type"
    FromOperator op ->
      "Operands of '" <> op <> "' must have the same type"
    FromFunctionArgument fn 1 _ ->
      "Wrong type for the 1st argument to '" <> fn <> "'"
    FromFunctionArgument fn 2 _ ->
      "Wrong type for the 2nd argument to '" <> fn <> "'"
    FromFunctionArgument fn 3 _ ->
      "Wrong type for the 3rd argument to '" <> fn <> "'"
    FromFunctionArgument fn n _ ->
      "Wrong type for the " <> toOrdinal n <> " argument to '" <> fn <> "'"
    FromFunctionReturn fn ->
      "Return type of '" <> fn <> "' doesn't match its annotation"
    FromIfCondition ->
      "The 'if' condition must be Boolean, not " <> inlineOrDescribe found
    FromWhileCondition ->
      "The 'while' condition must be Boolean, not " <> inlineOrDescribe found
    FromIfBranches ThenBranch ->
      "The 'then' branch returns a different type than 'else'"
    FromIfBranches ElseBranch ->
      "The 'else' branch returns a different type than 'then'"
    FromListElement n | n > 0 ->
      "The " <> toOrdinal n <> " list element has a different type"
    FromListElement _ ->
      "All list elements must have the same type"
    FromTypeAnnotation ->
      case arityMismatch found expected of
        Just (nFound, nExpected) ->
          "This function takes " <> countArguments nFound <> ", but its annotation says it takes " <> show nExpected

        Nothing ->
          typeMismatchTitle found expected
    FromPatternMatch n | n > 0 ->
      "The " <> toOrdinal n <> " branch of 'where' returns a different type"
    FromPatternMatch _ ->
      "Branches of 'where' return different types"
    FromAssignment name ->
      "Cannot assign " <> inlineOrDescribe found <> " to '" <> name <> "'"
    NoOrigin ->
      typeMismatchTitle found expected


-- | Title for a unification failure that names the two types. Guarantees the
-- two sides never render identically: when their short renderings collide,
-- the title names the location of the first structural difference instead
-- of naming any type.
typeMismatchTitle :: Type -> Type -> String
typeMismatchTitle found expected =
  let foundStr    = inlineOrDescribe found
      expectedStr = inlineOrDescribe expected
  in  if foundStr /= expectedStr then
        "Type mismatch: expected " <> expectedStr <> " but found " <> foundStr
      else
        case firstDifference found expected of
          Just place ->
            "Type mismatch in " <> place

          Nothing ->
            "Type mismatch"


-- | The full one-line rendering of a type if it is short enough for a title,
-- otherwise a structural description via 'describeType'.
inlineOrDescribe :: Type -> String
inlineOrDescribe t =
  let full = prettyPrintType True t
  in  if length full <= 28 && '\n' `notElem` full then full else describeType t


-- | A short structural description of a type. Unlike a bare constructor
-- name it always carries a distinguishing detail (arity, field names,
-- element count) so that two different types rarely describe identically.
describeType :: Type -> String
describeType t
  | isFunctionType t
  = "a function taking " <> countArguments (length (getParamTypes t))
  | isTuple t
  = "a tuple of " <> show (countTypeArgs t) <> " elements"
  | otherwise
  = case t of
      TCon (TC name _) _ _ ->
        name

      TApp (TCon (TC "List" _) _ _) inner ->
        "List " <> describeType inner

      TRecord fields _ optionalFields ->
        let names = M.keys (fields <> optionalFields)
            shown = take 3 names
            more  = length names - length shown
        in  "a record with field" <> (if length names == 1 then " " else "s ")
              <> List.intercalate ", " (("'" <>) . (<> "'") <$> shown)
              <> (if more > 0 then " and " <> show more <> " more" else "")

      TVar _ ->
        "a polymorphic type variable"

      _ ->
        prettyPrintType True t
  where
    countTypeArgs ty = case ty of
      TApp l _ ->
        1 + countTypeArgs l

      _ ->
        0


-- | Both types are functions with a different number of parameters.
arityMismatch :: Type -> Type -> Maybe (Int, Int)
arityMismatch found expected
  | isFunctionType found
  , isFunctionType expected
  , let nFound    = length (getParamTypes found)
  , let nExpected = length (getParamTypes expected)
  , nFound /= nExpected
  = Just (nFound, nExpected)
  | otherwise
  = Nothing


countArguments :: Int -> String
countArguments n =
  show n <> " argument" <> (if n == 1 then "" else "s")


-- | Maps commonly-used stdlib names to their module.
stdlibMap :: [(String, String)]
stdlibMap =
  [ ("map",        "List")
  , ("filter",     "List")
  , ("reduce",     "List")
  , ("length",     "List")
  , ("head",       "List")
  , ("last",       "List")
  , ("tail",       "List")
  , ("reverse",    "List")
  , ("concat",     "List")
  , ("append",     "List")
  , ("zip",        "List")
  , ("unzip",      "List")
  , ("find",       "List")
  , ("any",        "List")
  , ("all",        "List")
  , ("sum",        "List")
  , ("product",    "List")
  , ("sort",       "List")
  , ("sortBy",     "List")
  , ("log",        "IO")
  , ("print",      "IO")
  , ("readLine",   "IO")
  , ("fromMaybe",  "Maybe")
  , ("isJust",     "Maybe")
  , ("isNothing",  "Maybe")
  , ("fromJust",   "Maybe")
  , ("just",       "Maybe")
  , ("nothing",    "Maybe")
  , ("catMaybes",  "Maybe")
  , ("split",      "String")
  , ("join",       "String")
  , ("trim",       "String")
  , ("toLower",    "String")
  , ("toUpper",    "String")
  , ("replace",    "String")
  , ("slice",      "String")
  , ("parseInt",   "Number")
  , ("parseFloat", "Number")
  , ("fromString", "Number")
  , ("toString",   "Show")
  , ("floor",      "Math")
  , ("ceil",       "Math")
  , ("round",      "Math")
  , ("sqrt",       "Math")
  , ("abs",        "Math")
  , ("min",        "Math")
  , ("max",        "Math")
  , ("pow",        "Math")
  , ("random",     "Random")
  ]


-- | Generate context-aware hints for NoInstanceFound errors.
-- These are shown in addition to the standard "implement the interface" hint.
noInstanceSmartHints :: String -> [Type] -> [Note]
noInstanceSmartHints cls ts = case (cls, ts) of
  -- Number interface — concrete actionable fixes
  ("Number", [TCon (TC "String" _) _ _]) ->
    [ hint "Strings are not numbers. Use '<>' to concatenate strings instead of '+'."
    , note "To parse a String as a number, use Number.fromString which returns a Maybe Number."
    ]
  ("Number", [TCon (TC "Boolean" _) _ _]) ->
    [ hint "Booleans are not numbers. Use '&&' or '||' for boolean logic."
    , note "To convert Boolean to Int, write: if condition then 1 else 0"
    ]
  ("Number", [TCon (TC "Char" _) _ _]) ->
    [ hint "Characters are not numbers. Use 'Char.toInt' to get the Unicode code point."
    , note "Example: Char.toInt('A') == 65"
    ]
  ("Number", [TApp (TCon (TC "List" _) _ _) _]) ->
    [ hint "Lists are not numbers. Did you mean 'List.length' to count elements?"
    , note "Or 'List.sum' / 'List.product' if you want to reduce numeric elements."
    ]
  ("Number", [TApp (TApp (TCon (TC "(->)" _) _ _) _) _]) ->
    [ hint "A function is not a number — you may have forgotten to apply it to its arguments."
    , note "Example: instead of 'compute + 1', write 'compute(input) + 1'"
    ]
  -- Eq interface
  ("Eq", [TCon (TC name _) _ _]) ->
    [ hint $ "Add 'derive Eq' to the '" <> name <> "' type definition to get equality for free."
    , note $ "Example:  type " <> name <> " = " <> name <> " { ... } deriving Eq"
    ]
  ("Eq", _) ->
    [ hint "Add 'derive Eq' to your type definition to auto-generate equality."
    , note "All built-in types (Number, String, Boolean, Char) already implement Eq."
    ]
  -- Show interface
  ("Show", [TCon (TC name _) _ _]) ->
    [ hint $ "Add 'derive Show' to the '" <> name <> "' type definition to enable string conversion."
    , note $ "Example:  type " <> name <> " = " <> name <> " { ... } deriving Show"
    ]
  ("Show", _) ->
    [ hint "Add 'derive Show' to your type definition to auto-generate Show."
    , note "All built-in types already implement Show."
    ]
  -- Comparable interface
  ("Comparable", [TCon (TC name _) _ _]) ->
    [ hint $ "Add 'derive Comparable' to '" <> name <> "' to enable sorting and ordering."
    , note $ "This allows using '" <> name <> "' with '<', '>', 'List.sortBy', 'List.minimum', etc."
    ]
  ("Comparable", _) ->
    [ hint "Add 'derive Comparable' to your type to enable ordering operators."
    , note "Comparable is needed for: '<', '>', '<=', '>=', 'List.sortBy', 'List.minimum', 'List.maximum'."
    ]
  -- Monad/Apply/Functor
  ("Functor", _) ->
    [ hint "Implement 'instance Functor YourType' with a 'map' method."
    , note "Functor is required for 'map', which applies a function to the value inside a container."
    ]
  ("Monad", _) ->
    [ hint "Implement 'instance Monad YourType' with 'of' and 'chain' methods."
    , note "'chain' is equivalent to 'flatMap'/'bind'. 'of' wraps a value in the monad."
    ]
  ("Apply", _) ->
    [ hint "Implement 'instance Apply YourType' with an 'ap' method."
    , note "'ap' applies a function inside a container to a value inside a container."
    ]
  _ -> []


-- | Generate extra hints based on the megaparsec error message text.
grammarSmartHints :: String -> [Note]
grammarSmartHints msg
  | "unexpected end of input" `List.isInfixOf` msg =
      [hint "Something is missing — a closing bracket, a missing expression, or an incomplete statement."]
  | "unexpected whitespace" `List.isInfixOf` msg =
      [note "The indentation or spacing here is unexpected."]
  | "unexpected =\n" `List.isInfixOf` msg || msg == "unexpected =\n         expecting end of input" || "unexpected =" `List.isPrefixOf` msg =
      [ hint "If you are trying to mutate a variable, use ':=' instead of '='."
      , note "Top-level bindings use '='. Inside a block, use ':=' to reassign."
      ]
  | "unexpected :" `List.isPrefixOf` msg && not ("::" `List.isInfixOf` msg) =
      [hint "Did you mean '::' for a type annotation, or ':=' for mutation?"]
  | "unexpected identifier" `List.isInfixOf` msg =
      [note "An identifier appeared where it wasn't expected. Check for a missing operator or comma."]
  | otherwise = []


-- | Generate operator-specific hints for UnificationError.
operatorHints :: String -> Type -> Type -> [Note]
operatorHints op found _expected = case op of
  "&&" -> boolOpHints "&&" found
  "||" -> boolOpHints "||" found
  "+"  ->
    case found of
      TCon (TC "String" _) _ _ ->
        [ hint "Use '<>' to concatenate strings: a <> b"
        , note "'+' only works on numeric types (Number, Integer, Float, Short, Byte)."
        ]
      _ ->
        [ hint "Both sides of '+' must have the same numeric type."
        , note "Use '<>' to concatenate strings."
        ]
  "++" -> [ hint "Both sides of '++' must be lists of the same element type." ]
  "<>" -> [ hint "Both sides of '<>' must have the same type (e.g. both String, or both List)." ]
  _    -> [ hint $ "Both operands of '" <> op <> "' must be the same type." ]
  where
    boolOpHints :: String -> Type -> [Note]
    boolOpHints opName t = case t of
      TCon (TC "String" _) _ _ ->
        [ hint $ "'" <> opName <> "' requires Boolean, not String. Did you mean to compare with '=='?"
        , note "Example: instead of 'cond && str', write 'cond && str == expectedValue'"
        ]
      TCon (TC "Integer" _) _ _ ->
        [ hint $ "'" <> opName <> "' requires Boolean, not Integer. Did you mean to compare with '== 0'?"
        , note "Example: instead of 'cond && n', write 'cond && n != 0'"
        ]
      TCon (TC tname _) _ _ ->
        [ hint $ "'" <> opName <> "' requires Boolean on both sides, but got " <> tname <> "." ]
      _ ->
        [ hint $ "Both sides of '" <> opName <> "' must be Boolean." ]
