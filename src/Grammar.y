{
module Grammar where
import Lexer
import Text.Printf
import Control.Monad.Except
import Type
import qualified Data.Map as M
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
  js       { Token _ (TokenJSBlock _) }
  'ret'    { Token _ TokenReturn }
  '='      { Token _ TokenEq }
  '+'      { Token _ TokenPlus }
  '-'      { Token _ TokenDash }
  '*'      { Token _ TokenStar }
  '/'      { Token _ TokenSlash }
  '::'     { Token _ TokenDoubleColon }
  ':'      { Token _ TokenColon }
  '->'     { Token _ TokenArrow }
  '=>'     { Token _ TokenFatArrow }
  '.'      { Token _ TokenDot }
  ','      { Token _ TokenComa }
  '('      { Token _ TokenLeftParen }
  ')'      { Token _ TokenRightParen }
  '{'      { Token _ TokenLeftCurly }
  '}'      { Token _ TokenRightCurly }
  '['      { Token _ TokenLeftSquaredBracket }
  ']'      { Token _ TokenRightSquaredBracket }
  'if'     { Token _ TokenIf }
  'else'   { Token _ TokenElse }
  '==='    { Token _ TokenTripleEq }
  false    { Token _ (TokenBool _) }
  true     { Token _ (TokenBool _) }
  'import' { Token _ TokenImport }
  'export' { Token _ TokenExport }
  'from'   { Token _ TokenFrom }
  '|'      { Token _ TokenPipe }
  '|>'     { Token _ TokenPipeOperator }
  'data'   { Token _ TokenData }

%left '==='
%left '+' '-'
%left '*' '/'
%right ','
%nonassoc '=' '=>' '::' ':'
%left '->' '|' '|>'
%%

ast :: { AST }
  : adt ast          %shift { $2 { aadts =  [$1] <> aadts $2 } }
  | adt              %shift { AST { aimports = [], aexps = [], aadts = [$1], apath = Nothing } }
  | exp ast          %shift { $2 { aexps = [$1] <> aexps $2 } }
  | exp              %shift { AST { aimports = [], aexps = [$1], aadts = [], apath = Nothing } }
  | importDecls ast  %shift { $2 { aimports = $1, apath = Nothing } }
  | {- empty -}      %shift { AST { aimports = [], aexps = [], aadts = [], apath = Nothing } }
  | 'export' name '=' exp ast %shift { $5 {
                                       aexps =
                                       [Assignment
                                        { epos = tokenToPos $1
                                        , etype = Nothing
                                        , ename = strV $2
                                        , eexp = $4
                                        , eexported = True
                                        }] <> aexps $5
                                      }
                                     }
  | rRet              { AST { aimports = [], aexps = [], aadts = [], apath = Nothing } }
  | rRet ast          { $2 }
  | ast rRet          { $1 }

importDecls :: { [ImportDecl] }
  : importDecl importDecls { $1:$2 }
  | importDecl             { [$1] }
  
importDecl :: { ImportDecl }
  : 'import' '{' importNames '}' 'from' str rRet { ImportDecl { ipos = tokenToPos $1, inames = $3, ipath = strV $6 } }

importNames :: { [Name] }
  : name ',' importNames %shift { strV $1 : $3 }
  | name                 %shift { [strV $1] }

rRet :: { [TokenClass] }
  : 'ret'       { [] }
  -- | 'ret' rret { [] }

maybeRet :: { [TokenClass] }
  : 'ret'       { [] }
  | {- empty -} { [] }

rEq :: { [TokenClass] }
  : '='       { [] }
  | 'ret' '=' { [] }

rPipe :: { [TokenClass] }
  : '|'       { [] }
  | 'ret' '|' { [] }

rParenL :: { [TokenClass] }
  : '('       { [] }
  | '(' 'ret' { [] }

rComa :: { [TokenClass] }
  : ','       { [] }
  | ',' 'ret' { [] }
  | 'ret' ',' { [] }


adt :: { ADT }
  : 'data' name adtParameters rEq adtConstructors %shift { ADT { adtname = strV $2, adtparams = $3, adtconstructors = $5 } }
  | 'data' name adtParameters rEq adtRecordConstructors %shift { ADT { adtname = strV $2, adtparams = $3, adtconstructors = $5 } }

adtParameters :: { [Name] }
  : name adtParameters %shift { strV $1 : $2 }
  | name               %shift { [strV $1] }
  | {- empty -}               { [] }

adtConstructors :: { [ADTConstructor] }
  : adtConstructor rPipe adtConstructors      %shift { $1:$3 }
  | adtConstructor rRet                       %shift { [$1] }

adtConstructor :: { ADTConstructor }
  : name adtConstructorArgs %shift { ADTConstructor { adtcname = strV $1, adtcargs = $2 } }
  | name                    %shift { ADTConstructor { adtcname = strV $1, adtcargs = [] } }

adtConstructorArgs :: { [TypeRef] }
  : name adtConstructorArgs                                 { (TRSingle $ strV $1) : $2 }
  | name '(' name adtConstructorArgs ')' adtConstructorArgs { (TRSingle $ strV $1) : TRComp (strV $3) $4 : $6 }
  | '(' name ')' adtConstructorArgs                         { (TRSingle $ strV $1) : $4 }
  | '(' name adtConstructorArgs ')'                         { [TRComp (strV $2) $3] }
  | '(' name ')'                                            { [TRSingle $ strV $2] }
  | name                                                    { [TRSingle $ strV $1] }

adtRecordConstructors :: { [ADTConstructor] }
  : adtRecordConstructor rPipe adtRecordConstructors %shift { $1:$3 }
  | adtRecordConstructor rRet                        %shift { [$1] }

adtRecordConstructor :: { ADTConstructor }
  : name '{' adtRecordConstructorFields '}' %shift { ADTRecordConstructor { adtcname = strV $1, adtcfields = $3 } }

adtRecordConstructorFields :: { [ADTRecordConstructorField] }
  : adtRecordConstructorField ',' adtRecordConstructorFields { $1:$3 }
  | adtRecordConstructorField                                { [$1] }

adtRecordConstructorField :: { ADTRecordConstructorField }
  : name '::' type { ADTRecordConstructorField { adtrcfname = strV $1, adtrcftype = $3 } }

type :: { TypeRef }
  : name              { TRSingle $ strV $1 }
  | name type         { TRComp (strV $1) [$2] }
  | name '(' type ')' { TRComp (strV $1) [$3] }
  | type '->' type    { TRArr $1 $3 }

exp :: { Exp }
  : literal                         { $1 }
  | operation                       { $1 }
  | listConstructor          %shift { $1 }
  | js                       %shift { JSExp { epos = tokenToPos $1, etype = Just TAny, econtent = strV $1 } }
  | name '=' exp             %shift { Assignment { epos = tokenToPos $1, etype = Nothing, ename = strV $1, eexp = $3, eexported = False }}
  | name                     %shift { Var { epos = tokenToPos $1, etype = Nothing, ename = strV $1 }}
  | name rParenL args ')'    %shift { buildApp (tokenToPos $1) Var { epos = tokenToPos $1, etype = Nothing, ename = strV $1 } $3 }
  | exp '(' args ')'         %shift { buildApp (epos $1) $1 $3 }
  | '(' exp ')' '(' args ')' %shift { buildApp (epos $2) $2 $5 }
  | '(' params ')' '=>' exp  %shift { buildAbs (tokenToPos $1) $2 $5 }
  | '(' exp ')'              %shift { $2 }
  | exp '::' typings                { TypedExp { epos = epos $1, etype = Nothing, eexp = $1, etyping = $3 } }
  | recordCall                      { $1 }
  | exp '.' name                    { App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $3, etype = Nothing, ename = strV $3 }, earg = $1, efieldAccess = True } }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}' {
    App { epos = tokenToPos $1, etype = Nothing, eabs =
      App { epos = tokenToPos $1, etype = Nothing, eabs = 
        App { epos = tokenToPos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "ifElse" }, earg = $3, efieldAccess = False }
    , earg = $7, efieldAccess = False }, earg = $15, efieldAccess = False
    }
  }


operation :: { Exp }
  : exp '+' exp  { App { epos = epos $1
                       , etype = Nothing
                       , eabs = App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "+" }, earg = $1, efieldAccess = False }
                       , earg = $3
                       , efieldAccess = False 
                       }
                 }
  | exp '-' exp  { App { epos = epos $1
                       , etype = Nothing
                       , eabs = App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "-" }, earg = $1, efieldAccess = False }
                       , earg = $3
                       , efieldAccess = False 
                       }
                 }
  | exp '*' exp  { App { epos = epos $1
                       , etype = Nothing
                       , eabs = App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "*" }, earg = $1, efieldAccess = False }
                       , earg = $3
                       , efieldAccess = False 
                       }
                 }
  | exp '/' exp  { App { epos = epos $1
                       , etype = Nothing
                       , eabs = App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "/" }, earg = $1, efieldAccess = False }
                       , earg = $3
                       , efieldAccess = False 
                       }
                 }
  | exp '===' exp  { App { epos = epos $1
                         , etype = Nothing
                         , eabs = App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "===" }, earg = $1, efieldAccess = False }
                         , earg = $3
                         , efieldAccess = False 
                         }
                   }
  | exp '|>' exp  { App { epos = epos $1
                        , etype = Nothing
                        , eabs = App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "|>" }, earg = $1, efieldAccess = False }
                        , earg = $3
                        , efieldAccess = False 
                        }
                  }

listConstructor :: { Exp }
  : '[' listItems ']' { ListConstructor { epos = tokenToPos $1, etype = Nothing, eelems = $2 } }

listItems :: { [Exp] }
  : exp               { [$1] }
  | exp ',' listItems { $1 : $3 }

recordCall :: { Exp }
  : name '{' fieldAssignments '}' { RecordCall { epos = tokenToPos $1, etype = Nothing, ename = strV $1, efields = $3 } }

fieldAssignments :: { M.Map String Exp }
  : name ':' exp                      { M.fromList [(strV $1, $3)] }
  | name ':' exp ',' fieldAssignments { M.insert (strV $1) $3 $5 }

literal :: { Exp }
  : int                       { LInt  { epos = tokenToPos $1, etype = Nothing, eval = strV $1 } }
  | str                       { LStr  { epos = tokenToPos $1, etype = Nothing, eval = strV $1 } }
  | false                     { LBool { epos = tokenToPos $1, etype = Nothing, eval = strV $1 } }
  | true                      { LBool { epos = tokenToPos $1, etype = Nothing, eval = strV $1 } }

args :: { [Exp] }
  : exp rComa args %shift { $1:$3 }
  | exp maybeRet   %shift { [$1] }

params :: { [Name] }
  : name ',' params %shift { strV $1 : $3 }
  | name                   { [strV $1] }

typings :: { TypeRef }
  : typing '->' typings { TRArr $1 $3 }
  | typing              { $1 }

typing :: { TypeRef }
  : name                       { TRSingle $ strV $1 }
  | '(' compositeTyping ')'    { $2 }
  |  compositeTyping           { $1 }
  | '(' typing '->' typing ')' { TRArr $2 $4 }

compositeTyping :: { TypeRef }
  : name compositeTypingArgs { TRComp (strV $1) $2 }

compositeTypingArgs :: { [TypeRef] }
  : name                     { [TRSingle $ strV $1] }
  | name compositeTypingArgs { (TRSingle $ strV $1) : $2 }

{
buildAbs :: Pos -> [Name] -> Exp -> Exp
buildAbs pos [param] body = Abs { epos = pos, etype = Nothing, eparam = param, ebody = body }
buildAbs pos (x:xs) body  = Abs { epos = pos, etype = Nothing, eparam = x, ebody = buildAbs pos xs body }

buildApp :: Pos -> Exp -> [Exp] -> Exp
buildApp pos f [arg]  = App { epos = pos, etype = Nothing, eabs = f, earg = arg, efieldAccess = False }
buildApp pos f xs = App { epos = pos, etype = Nothing, eabs = buildApp pos f (init xs) , earg = last xs, efieldAccess = False }

data AST =
  AST
    { aimports   :: [ImportDecl]
    , aexps      :: [Exp]
    , aadts       :: [ADT]
    , apath      :: Maybe FilePath
    }
    deriving(Eq, Show)

data ImportDecl =
  ImportDecl
    { ipos   :: Pos
    , inames :: [Name]
    , ipath  :: FilePath
    }
    deriving(Eq, Show)

-- TODO: Add Pos
data ADT = ADT { adtname :: Name, adtparams :: [Name], adtconstructors :: [ADTConstructor] } deriving(Eq, Show)

-- TODO: Add Pos
data ADTConstructor
  = ADTConstructor       { adtcname :: Name, adtcargs :: [TypeRef] }
  -- TODO: Should the fields be a Map ?
  | ADTRecordConstructor { adtcname :: Name, adtcfields :: [ADTRecordConstructorField] }
  deriving(Eq, Show)

data ADTRecordConstructorField =
  ADTRecordConstructorField
    { adtrcfname :: Name
    , adtrcftype :: TypeRef
    }
    deriving(Eq, Show)

-- TODO: Rename
data TypeRef
  = TRSingle Name
  | TRComp Name [TypeRef]
  | TRArr TypeRef TypeRef
  deriving(Eq, Show)

data Exp = LInt            { epos :: Pos, etype :: Maybe Type, eval :: String }
         | LStr            { epos :: Pos, etype :: Maybe Type, eval :: String }
         | LBool           { epos :: Pos, etype :: Maybe Type, eval :: String }
         | JSExp           { epos :: Pos, etype :: Maybe Type, econtent :: String }
         | App             { epos :: Pos, etype :: Maybe Type, eabs :: Exp, earg :: Exp, efieldAccess :: Bool }
         | Abs             { epos :: Pos, etype :: Maybe Type, eparam :: Name, ebody :: Exp }
         | Assignment      { epos :: Pos, etype :: Maybe Type, eexp :: Exp, ename :: Name, eexported :: Bool }
         | Var             { epos :: Pos, etype :: Maybe Type, ename :: Name }
         | TypedExp        { epos :: Pos, etype :: Maybe Type, eexp :: Exp, etyping :: TypeRef }
         | RecordCall      { epos :: Pos, etype :: Maybe Type, ename :: Name, efields :: M.Map String Exp }
         | ListConstructor { epos :: Pos, etype :: Maybe Type, eelems :: [Exp] }
         deriving(Eq, Show)

type Name  = String

newtype Typing = Typing String deriving(Eq, Show)


lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap f = alexMonadScan >>= f

parseError :: Token -> Alex a
parseError (Token (Pos a l c) cls) =
  alexError (printf "Syntax error - line: %d, column: %d\nThe following token is not valid: %s" l c (show cls))

parse :: String -> Either String AST
parse s = runAlex s parseMadlib
}
