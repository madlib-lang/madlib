{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.State.Lazy
import           Data.Either
import qualified Data.List                as L
import qualified Data.Map                 as M
import           Debug.Trace
import           Grammar
import           Lexer
import           Text.Printf

main :: IO ()
main = interact (\x -> generateOutput (buildAST x >>= analyze))

-- data Analyzed = Signature Typing
type Names = M.Map String [String]
newtype Analyzer a = Analyzer { runAnalyzer :: StateT Names (Either String) a } deriving (Functor, Applicative, Monad, MonadState Names, MonadError String)

buildAST :: String -> Either String [Exp]
buildAST x =
  case parse x of
    Left a  -> Left ("\nERR:\n" ++ a ++ "\n")
    Right b -> trace ("\nAST:\n" ++ show b ++ "\n") Right b

analyze :: [Exp] -> Either String [Exp]
analyze e = (
              (\x -> case x of
                (Right (f, s)) -> trace (show s) (Right f)
                (Left s)       -> Left s
              )
            . runStateT (runAnalyzer (runExps e))
            ) M.empty

runExps :: [Exp] -> Analyzer [Exp]
runExps ex = foldl analyzeExp' initialState ex where
  initialState    = return ex
  analyzeExp' a b = Analyzer $ mapStateT (analyzeExp a b) (runAnalyzer a)

analyzeExp :: Analyzer [Exp] -> Exp ->  Either String ([Exp], Names) -> Either String ([Exp], Names)
analyzeExp ae (Typing _ n t) (Right (o, p))                = Right (o, M.insert n (fmap typeToName t) p)
analyzeExp ae (FunctionDeclaration (TokenPos _ l c) n p b) (Right (r, s)) = case M.lookup n s of
                                                               (Just x) -> if validateFn p x b
                                                                           then Right (r, s)
                                                                           else Left $ printf "function %s did has a type problem.\n" n
                                                               _        -> Left $ printf "%d:%d - function %s does not have a type definition.\n" l c n
analyzeExp _ _ o                                           = o

validateFn :: [Param] -> [String] -> Body -> Bool
validateFn p b (Body (BinaryTerm (Variable x) Plus (UnaryTerm (Variable y)))) =
  let (Just iX) = L.findIndex (\(Param pn) -> pn == x) p
      (Just iY) = L.findIndex (\(Param pn) -> pn == y) p
  in b !! iX == "Int" && b !! iY == "Int"
validateFn _ _ _ = False


typeToName :: Type -> String
typeToName (Type n) = n

generateOutput :: Either String [Exp] -> String
generateOutput (Left a)  = "ERR: " ++ a
generateOutput (Right b) = show b
