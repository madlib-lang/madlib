module AST.Canonical where

-- It should convert TypedExps and assign them directly to functions
-- in this situation:
-- fn :: A -> B
-- fn a = ...
-- Currently we assign the TypedExp to a Var that has the parsed type
-- and then lookup that type during assignment if it is an Abs to see
-- if the types match.

-- Consider already parsing ADTs and typings during that phase ?