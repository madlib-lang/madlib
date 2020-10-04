{
module Grammar where
import Lexer
import Text.Printf
import Control.Monad.Except
}

%name parseMadlib ast
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexerWrap } { Token _ TokenEOF }

%token
  int      { Token _ (TokenInt _) }
  str      { Token _ (TokenStr _) }
  name     { Token _ (TokenName _) }
  '='      { Token _ TokenEq }
  '+'      { Token _ TokenPlus }
  '::'     { Token _ TokenDoubleColon }
  '->'     { Token _ TokenArrow }
  '=>'     { Token _ TokenFatArrow }
  ','      { Token _ TokenComa }
  '('      { Token _ TokenLeftParen }
  ')'      { Token _ TokenRightParen }
  '==='    { Token _ TokenTripleEq }
  false    { Token _ (TokenBool _) }
  true     { Token _ (TokenBool _) }
  'import' { Token _ TokenImport }
%%

ast :: { AST }
  : importDecls exps { AST { aimports = $1, aexps = $2, apath = Nothing } }
  | exps             { AST { aimports = [], aexps = $1, apath = Nothing } }

importDecls :: { [ImportDecl] }
  : importDecl importDecls { $1:$2 }
  | importDecl             { [$1] }
  
importDecl :: { ImportDecl }
  : 'import' str { ImportDecl { ipos = tokenToPos $1, ipath = strV $2 } }

exps :: { [Exp] }
  : exp exps { $1:$2 }
  | exp      { [$1] }

exp :: { Exp }
--   : name '=' '(' params ')' '=>' exp         { Abs
--                                                   { epos = tokenToPos $1
--                                                   , ename = strV $1
--                                                   , eparam = $4
--                                                   , ebody = $7 
--                                                   }
--                                               }
-- Abs should also have some named version that would be available in the current scope ( most likely module scope but could potentially be defined at different levels )
  : '(' params ')' '=>' exp   { buildAbs (tokenToPos $1) $2 $5 }
  | int               { LInt  { epos = tokenToPos $1} }
  | str               { LStr  { epos = tokenToPos $1} }
  | false             { LBool { epos = tokenToPos $1} }
  | true              { LBool { epos = tokenToPos $1} }
-- Make it an App of an App with the first one being a Var { ename = operatorName }, these should be in the initialEnv of inference with types being set
-- Example App { eabs = Var { ename = "+" }, earg = App { ... } }
-- initialEnv = fromList [("+", Type (Num -> Num -> Num))]
--   | exp operator exp  { App   { epos = epos $1, eleft = $1, eoperator = $2, eright = $3 }}
  | name              { Var   { epos = tokenToPos $1, ename = strV $1 }}
  -- That one should recursively Apply values like Abs
  | '(' exp ')' '(' exp ')' { App   { epos = epos $2, eabs = $2, earg = $5 }}
  | exp '(' exp ')' { App   { epos = epos $1, eabs = $1, earg = $3 }}
--   | name '(' args ')' { App   { epos = tokenToPos $1, eabs = Var { epos = tokenToPos $1, ename = strV $1 }, earg = $3 }}

args :: { [Exp] }
  : exp          { [$1] }
  | exp ',' args { $1:$3 }

params :: { [Name] }
  : name ',' params { (strV $1) : $3 }
  | name            { [(strV $1)] }

body :: { Body }
  : exp { Body $1 }

operator :: { Operator }
  : '===' { TripleEq }
  | '+'   { Plus }

{
buildAbs :: Pos -> [Name] -> Exp -> Exp
buildAbs pos [arg] body = Abs { epos = pos, eparam = arg, ebody = body }
buildAbs pos (x:xs) body  = Abs { epos = pos, eparam = x, ebody = buildAbs pos xs body }

data AST =
  AST
    { aimports   :: [ImportDecl]
    , aexps      :: [Exp]
    , apath      :: Maybe FilePath
    }
    deriving(Eq, Show)

data ImportDecl =
  ImportDecl
    { ipos  :: Pos
    , ipath :: FilePath
    }
    deriving(Eq, Show)

data Exp = LInt  { epos :: Pos }
         | LStr  { epos :: Pos }
         | LBool { epos :: Pos }
         | App   { epos :: Pos, eabs :: Exp, earg :: Exp } -- HM Application, first exp should be an Abstraction
         | Abs   { epos :: Pos, eparam :: Name, ebody :: Exp }
         | Var   { epos :: Pos, ename :: Name }
         deriving(Eq, Show)

type Name  = String

data Body = Body Exp deriving(Eq, Show)

data Operator = TripleEq 
              | Plus
              deriving(Eq, Show)

lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap f = alexMonadScan >>= f

parseError :: Token -> Alex a
parseError (Token (Pos a l c) cls) =
  alexError (printf "Syntax error - line: %d, column: %d\nThe following token is not valid: %s" l c (show cls))

parse :: String -> Either String AST
parse s = runAlex s parseMadlib
}
