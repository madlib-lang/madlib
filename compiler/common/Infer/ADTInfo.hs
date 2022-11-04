module Infer.ADTInfo where

import qualified AST.Solved as Slv

data ADTInfo = ADTInfo { numAlts :: Int, ctors :: [Slv.Constructor] }
