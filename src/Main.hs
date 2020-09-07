module Main where

import           Debug.Trace
import           Grammar
import           Lexer
import           Data.Either
import           Control.Monad.State.Lazy

main :: IO ()
main = interact (\x -> generateOutput (buildAST x >>= analyze))

buildAST :: String -> Either String [Exp]
buildAST x =
  case parse x of
    Left a  -> Left ("\nERR:\n" ++ a ++ "\n")
    Right b -> trace ("\nAST:\n" ++ show b ++ "\n") Right b

analyze :: [Exp] -> Either String String
analyze e = ( fst
            . (\(x, s) -> trace (show s ++ show x) (x, s))
            . runState (runExps e)
            ) []

runExps :: [Exp] -> State [String] (Either String String)
runExps = foldl (flip analyzeExp) (state $ const (Right "", []))

-- Should this embed the state and return a (returnValue, nextState) ?
analyzeExp :: Exp -> State [String] (Either String String) -> State [String] (Either String String)
analyzeExp (AssignmentExpression p _ _)  =  mapState (\(a, s) -> (a, s <> [show p]))
analyzeExp (ConditionExpression p _)     =  mapState (\(a, s) -> (a, s <> [show p]))
analyzeExp (Typing p _ _)                =  mapState (\(a, s) -> (a, s <> [show p]))
analyzeExp (FunctionDeclaration p _ _ _) =  mapState (\(a, s) -> (Left "Tchou", s <> [show p]))

generateOutput :: Either String String -> String
generateOutput (Left a)  = "ERR: " ++ a
generateOutput (Right b) = b
