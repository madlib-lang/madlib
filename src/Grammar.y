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
  '-'      { Token _ TokenDash }
  '*'      { Token _ TokenStar }
  '/'      { Token _ TokenSlash }
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
  ';'      { Token _ TokenSemiColon }

%left ';'
%right ','
%nonassoc '=' '=>'
%left '->'
%left '==='
%left '*' '/'
%left '+' '-'

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
  : exp exps     { $1:$2 }
  | exp ';' exps { $1:$3 }
  | exp          { [$1] }
  | exp ';'      { [$1] }

exp :: { Exp }
  : literal                         { $1 }
  | name                     %shift { Var { epos = tokenToPos $1, ename = strV $1 }}
  | exp operator exp         %shift { App { epos = epos $1, eabs = App { epos = epos $1, eabs = $2, earg = $1 }, earg = $3 }}
  | name '(' args ')'        %shift { buildApp (tokenToPos $1) Var { epos = tokenToPos $1, ename = strV $1 } $3 }
  | exp '(' args ')'         %shift { buildApp (epos $1) $1 $3 }
  | '(' exp ')' '(' args ')' %shift { buildApp (epos $2) $2 $5 }
  | '(' params ')' '=>' exp  %shift { buildAbs (tokenToPos $1) $2 $5 }
  | name '=' exp             %shift { Assignment { epos = tokenToPos $1, ename = strV $1, eexp = $3 }}
  | '(' exp ')'              %shift { $2 }
  | exp '::' typings                { TypedExp { epos = epos $1, eexp = $1, etyping = $3 } }

literal :: { Exp }
  : int                       { LInt  { epos = tokenToPos $1} }
  | str                       { LStr  { epos = tokenToPos $1} }
  | false                     { LBool { epos = tokenToPos $1} }
  | true                      { LBool { epos = tokenToPos $1} }

args :: { [Exp] }
  : exp ',' args %shift { $1:$3 }
  | exp          %shift { [$1] }

params :: { [Name] }
  : name ',' params %shift { (strV $1) : $3 }
  | name                   { [(strV $1)] }

typings :: { [Typing] }
  : name '->' typings { Typing (strV $1) : $3 }
  | name              { [Typing (strV $1)] }

operator :: { Exp }
  : '===' { Var { epos = tokenToPos $1, ename = "===" } }
  | '+'   { Var { epos = tokenToPos $1, ename = "+" } }
  | '-'   { Var { epos = tokenToPos $1, ename = "-" } }
  | '*'   { Var { epos = tokenToPos $1, ename = "*" } }
  | '/'   { Var { epos = tokenToPos $1, ename = "/" } }

{
buildAbs :: Pos -> [Name] -> Exp -> Exp
buildAbs pos [param] body = Abs { epos = pos, eparam = param, ebody = body }
buildAbs pos (x:xs) body  = Abs { epos = pos, eparam = x, ebody = buildAbs pos xs body }

buildApp :: Pos -> Exp -> [Exp] -> Exp
buildApp pos f [arg]  = App { epos = pos, eabs = f, earg = arg }
buildApp pos f xs = App { epos = pos, eabs = buildApp pos f (init xs) , earg = last xs }

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

data Exp = LInt       { epos :: Pos }
         | LStr       { epos :: Pos }
         | LBool      { epos :: Pos }
         | App        { epos :: Pos, eabs :: Exp, earg :: Exp }
         | Abs        { epos :: Pos, eparam :: Name, ebody :: Exp }
         | Assignment { epos :: Pos, eexp :: Exp, ename :: Name }
         | Var        { epos :: Pos, ename :: Name }
         | TypedExp   { epos :: Pos, eexp :: Exp, etyping :: [Typing] }
         deriving(Eq, Show)

type Name  = String

newtype Typing = Typing String deriving(Eq, Show)

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
