{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Explain.Format.Hints
  ( noInstanceSmartHints
  , operatorHints
  , grammarSmartHints
  , stdlibMap
  , mkUnificationTitle
  , shortTypeName
  , toOrdinal
  ) where

import           Error.Error (ErrorOrigin(..), BranchSide(..))
import           Infer.Type
import qualified Error.Diagnose                as Diagnose
import qualified Data.List as List


-- | Generates a descriptive title for unification errors based on the origin context.
mkUnificationTitle :: Type -> Type -> ErrorOrigin -> String
mkUnificationTitle found expected origin =
  let foundName  = shortTypeName found
      expectName = shortTypeName expected
  in  case origin of
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
      "The 'if' condition must be Boolean, not " <> foundName
    FromIfBranches ThenBranch ->
      "The 'then' branch returns a different type than 'else'"
    FromIfBranches ElseBranch ->
      "The 'else' branch returns a different type than 'then'"
    FromListElement n | n > 0 ->
      "The " <> toOrdinal n <> " list element has a different type"
    FromListElement _ ->
      "All list elements must have the same type"
    FromTypeAnnotation ->
      "Type mismatch: " <> foundName <> " is not " <> expectName
    FromPatternMatch n | n > 0 ->
      "The " <> toOrdinal n <> " branch of 'where' returns a different type"
    FromPatternMatch _ ->
      "Branches of 'where' return different types"
    FromAssignment name ->
      "Cannot assign " <> foundName <> " to '" <> name <> "'"
    NoOrigin ->
      if foundName /= expectName
        then "Type mismatch: " <> foundName <> " is not " <> expectName
        else "Type mismatch"


-- | Returns a short human-readable type name for use in error titles.
shortTypeName :: Type -> String
shortTypeName t = case t of
  TCon (TC name _) _ _                              -> name
  TApp (TCon (TC "List" _) _ _) inner               -> "List " <> shortTypeName inner
  TApp (TApp (TCon (TC "(->)" _) _ _) _) _          -> "Function"
  TApp (TCon (TC "(,)" _) _ _) _                    -> "Tuple"
  TVar _                                            -> "a type variable"
  _                                                 -> "a type"


-- | Format an integer as an English ordinal: 1 -> "1st", 2 -> "2nd", 3 -> "3rd", 4 -> "4th" ...
toOrdinal :: Int -> String
toOrdinal n =
  let suffix = case n `mod` 100 of
        11 -> "th"  -- 11th, 111th, ...
        12 -> "th"  -- 12th
        13 -> "th"  -- 13th
        _  -> case n `mod` 10 of
                1 -> "st"
                2 -> "nd"
                3 -> "rd"
                _ -> "th"
  in  show n <> suffix


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
noInstanceSmartHints :: String -> [Type] -> [Diagnose.Note String]
noInstanceSmartHints cls ts = case (cls, ts) of
  -- Number interface — concrete actionable fixes
  ("Number", [TCon (TC "String" _) _ _]) ->
    [ Diagnose.Hint "Strings are not numbers. Use '<>' to concatenate strings instead of '+'."
    , Diagnose.Note "To parse a String as a number, use Number.fromString which returns a Maybe Number."
    ]
  ("Number", [TCon (TC "Boolean" _) _ _]) ->
    [ Diagnose.Hint "Booleans are not numbers. Use '&&' or '||' for boolean logic."
    , Diagnose.Note "To convert Boolean to Int, write: if condition then 1 else 0"
    ]
  ("Number", [TCon (TC "Char" _) _ _]) ->
    [ Diagnose.Hint "Characters are not numbers. Use 'Char.toInt' to get the Unicode code point."
    , Diagnose.Note "Example: Char.toInt('A') == 65"
    ]
  ("Number", [TApp (TCon (TC "List" _) _ _) _]) ->
    [ Diagnose.Hint "Lists are not numbers. Did you mean 'List.length' to count elements?"
    , Diagnose.Note "Or 'List.sum' / 'List.product' if you want to reduce numeric elements."
    ]
  ("Number", [TApp (TApp (TCon (TC "(->)" _) _ _) _) _]) ->
    [ Diagnose.Hint "A function is not a number — you may have forgotten to apply it to its arguments."
    , Diagnose.Note "Example: instead of 'compute + 1', write 'compute(input) + 1'"
    ]
  -- Eq interface
  ("Eq", [TCon (TC name _) _ _]) ->
    [ Diagnose.Hint $ "Add 'derive Eq' to the '" <> name <> "' type definition to get equality for free."
    , Diagnose.Note $ "Example:  type " <> name <> " = " <> name <> " { ... } deriving Eq"
    ]
  ("Eq", _) ->
    [ Diagnose.Hint "Add 'derive Eq' to your type definition to auto-generate equality."
    , Diagnose.Note "All built-in types (Number, String, Boolean, Char) already implement Eq."
    ]
  -- Show interface
  ("Show", [TCon (TC name _) _ _]) ->
    [ Diagnose.Hint $ "Add 'derive Show' to the '" <> name <> "' type definition to enable string conversion."
    , Diagnose.Note $ "Example:  type " <> name <> " = " <> name <> " { ... } deriving Show"
    ]
  ("Show", _) ->
    [ Diagnose.Hint "Add 'derive Show' to your type definition to auto-generate Show."
    , Diagnose.Note "All built-in types already implement Show."
    ]
  -- Comparable interface
  ("Comparable", [TCon (TC name _) _ _]) ->
    [ Diagnose.Hint $ "Add 'derive Comparable' to '" <> name <> "' to enable sorting and ordering."
    , Diagnose.Note $ "This allows using '" <> name <> "' with '<', '>', 'List.sortBy', 'List.minimum', etc."
    ]
  ("Comparable", _) ->
    [ Diagnose.Hint "Add 'derive Comparable' to your type to enable ordering operators."
    , Diagnose.Note "Comparable is needed for: '<', '>', '<=', '>=', 'List.sortBy', 'List.minimum', 'List.maximum'."
    ]
  -- Monad/Apply/Functor
  ("Functor", _) ->
    [ Diagnose.Hint "Implement 'instance Functor YourType' with a 'map' method."
    , Diagnose.Note "Functor is required for 'map', which applies a function to the value inside a container."
    ]
  ("Monad", _) ->
    [ Diagnose.Hint "Implement 'instance Monad YourType' with 'of' and 'chain' methods."
    , Diagnose.Note "'chain' is equivalent to 'flatMap'/'bind'. 'of' wraps a value in the monad."
    ]
  ("Apply", _) ->
    [ Diagnose.Hint "Implement 'instance Apply YourType' with an 'ap' method."
    , Diagnose.Note "'ap' applies a function inside a container to a value inside a container."
    ]
  _ -> []


-- | Generate extra hints based on the megaparsec error message text.
grammarSmartHints :: String -> [Diagnose.Note String]
grammarSmartHints msg
  | "unexpected end of input" `List.isInfixOf` msg =
      [Diagnose.Hint "Something is missing — a closing bracket, a missing expression, or an incomplete statement."]
  | "unexpected whitespace" `List.isInfixOf` msg =
      [Diagnose.Note "The indentation or spacing here is unexpected."]
  | "unexpected =\n" `List.isInfixOf` msg || msg == "unexpected =\n         expecting end of input" || "unexpected =" `List.isPrefixOf` msg =
      [ Diagnose.Hint "If you are trying to mutate a variable, use ':=' instead of '='."
      , Diagnose.Note "Top-level bindings use '='. Inside a block, use ':=' to reassign."
      ]
  | "unexpected :" `List.isPrefixOf` msg && not ("::" `List.isInfixOf` msg) =
      [Diagnose.Hint "Did you mean '::' for a type annotation, or ':=' for mutation?"]
  | "unexpected identifier" `List.isInfixOf` msg =
      [Diagnose.Note "An identifier appeared where it wasn't expected. Check for a missing operator or comma."]
  | otherwise = []


-- | Generate operator-specific hints for UnificationError.
operatorHints :: String -> Type -> Type -> [Diagnose.Note String]
operatorHints op found _expected = case op of
  "&&" -> boolOpHints "&&" found
  "||" -> boolOpHints "||" found
  "+"  ->
    case found of
      TCon (TC "String" _) _ _ ->
        [ Diagnose.Hint "Use '<>' to concatenate strings: a <> b"
        , Diagnose.Note "'+' only works on numeric types (Number, Integer, Float, Short, Byte)."
        ]
      _ ->
        [ Diagnose.Hint "Both sides of '+' must have the same numeric type."
        , Diagnose.Note "Use '<>' to concatenate strings."
        ]
  "++" -> [ Diagnose.Hint "Both sides of '++' must be lists of the same element type." ]
  "<>" -> [ Diagnose.Hint "Both sides of '<>' must have the same type (e.g. both String, or both List)." ]
  _    -> [ Diagnose.Hint $ "Both operands of '" <> op <> "' must be the same type." ]
  where
    boolOpHints :: String -> Type -> [Diagnose.Note String]
    boolOpHints opName t = case t of
      TCon (TC "String" _) _ _ ->
        [ Diagnose.Hint $ "'" <> opName <> "' requires Boolean, not String. Did you mean to compare with '=='?"
        , Diagnose.Note "Example: instead of 'cond && str', write 'cond && str == expectedValue'"
        ]
      TCon (TC "Integer" _) _ _ ->
        [ Diagnose.Hint $ "'" <> opName <> "' requires Boolean, not Integer. Did you mean to compare with '== 0'?"
        , Diagnose.Note "Example: instead of 'cond && n', write 'cond && n != 0'"
        ]
      TCon (TC tname _) _ _ ->
        [ Diagnose.Hint $ "'" <> opName <> "' requires Boolean on both sides, but got " <> tname <> "." ]
      _ ->
        [ Diagnose.Hint $ "Both sides of '" <> opName <> "' must be Boolean." ]
