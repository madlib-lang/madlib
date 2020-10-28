{
module Grammar where
import Lexer
import Text.Printf
import Control.Monad.Except
import Type
import qualified Data.Map as M
import Data.Char(isUpper)
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
  ','      { Token _ TokenComma }
  '('      { Token _ TokenLeftParen }
  ')'      { Token _ TokenRightParen }
  '{'      { Token _ TokenLeftCurly }
  '}'      { Token _ TokenRightCurly }
  '['      { Token _ TokenLeftSquaredBracket }
  ']'      { Token _ TokenRightSquaredBracket }
  'if'     { Token _ TokenIf }
  'else'   { Token _ TokenElse }
  'switch' { Token _ TokenSwitch }
  'case'   { Token _ TokenCase }
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
%right name
%nonassoc '=' '=>' '::' ':'
-- %nonassoc '=' '=>' '::' ':' '(' ')'
%left '->' '|' '|>' 'ret'
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
  : importNames ',' name %shift { $1 <> [strV $3] }
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

adtParameters :: { [Name] }
  : name adtParameters %shift { strV $1 : $2 }
  | name               %shift { [strV $1] }
  | {- empty -}               { [] }

adtConstructors :: { [ADTConstructor] }
  : adtConstructor rPipe adtConstructors      %shift { $1:$3 }
  | adtConstructor rRet                       %shift { [$1] }

adtConstructor :: { ADTConstructor }
  : name adtConstructorArgs %shift { ADTConstructor { adtcname = strV $1, adtcargs = Just $2 } }
  | name                    %shift { ADTConstructor { adtcname = strV $1, adtcargs = Nothing } }

typings :: { TypeRef }
  : typing '->' typings { TRArr $1 $3 }
  | typing              { $1 }

adtConstructorArgs :: { [TypeRef] }
  : typing                    { [$1] }
  | adtConstructorArgs typing { $1 <> [$2] }

typing :: { TypeRef }
  : name                       { TRSingle $ strV $1 }
  | '(' compositeTyping ')'    { $2 }
  | '(' typing '->' typing ')' { TRArr $2 $4 }
  | '{' recordTypingArgs '}'   { TRRecord $2 }

compositeTyping :: { TypeRef }
  : name compositeTypingArgs { TRComp (strV $1) $2 }

compositeTypingArgs :: { [TypeRef] }
  : name                     { [TRSingle $ strV $1] }
  | name compositeTypingArgs { (TRSingle $ strV $1) : $2 }

recordTypingArgs :: { M.Map Name TypeRef }
  : name '::' typing                      { M.fromList [(strV $1, $3)] }
  | recordTypingArgs ',' name '::' typing { M.insert (strV $3) $5 $1 }

type :: { TypeRef }
  : name              { TRSingle $ strV $1 }
  | name type         { TRComp (strV $1) [$2] }
  | name '(' type ')' { TRComp (strV $1) [$3] }
  | type '->' type    { TRArr $1 $3 }

exp :: { Exp }
  : literal                         { $1 }
  | record                          { $1 }
  | switch                          { $1 }
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
  | exp '.' name                    { App { epos = epos $1, etype = Nothing, eabs = Var { epos = tokenToPos $3, etype = Nothing, ename = "." <> strV $3 }, earg = $1, efieldAccess = True } }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}' {
    App { epos = tokenToPos $1, etype = Nothing, eabs =
      App { epos = tokenToPos $1, etype = Nothing, eabs = 
        App { epos = tokenToPos $1, etype = Nothing, eabs = Var { epos = tokenToPos $2, etype = Nothing, ename = "ifElse" }, earg = $3, efieldAccess = False }
    , earg = $7, efieldAccess = False }, earg = $15, efieldAccess = False
    }
  }


switch :: { Exp }
  : 'switch' '(' exp ')' '{' maybeRet cases maybeRet '}' { Switch { epos = tokenToPos $1, etype = Nothing, eexp = $3, ecases = $7 } }

cases :: { [Case] }
  : 'case' pattern ':' exp             { [Case { casepos = tokenToPos $1, casetype = Nothing, casepattern = $2, caseexp = $4 }] }
  | cases 'ret' 'case' pattern ':' exp { $1 <> [Case { casepos = tokenToPos $3, casetype = Nothing, casepattern = $4, caseexp = $6 }] }

pattern :: { Pattern }
  : nonCompositePattern { $1 }
  | compositePattern    { $1 }

nonCompositePattern :: { Pattern }
  : name             { nameToPattern $ strV $1 }
  | int              { PNum $ strV $1 }
  | str              { PStr $ strV $1 }
  | true             { PBool $ strV $1 }
  | false            { PBool $ strV $1 }
  | recordPattern    { $1 }
  | '(' pattern ')'  { $2 }

-- Constructor pattern pattern
compositePattern :: { Pattern }
  : name patterns %shift { PCtor (strV $1) $2 }

patterns :: { [Pattern] }
  : nonCompositePattern          { [$1] }
  | patterns nonCompositePattern { $1 <> [$2] }

recordPattern :: { Pattern }
  : '{' recordFieldPatterns '}' { PRecord $2 }

recordFieldPatterns :: { M.Map Name Pattern }
  : name ':' pattern { M.fromList [(strV $1, $3)] }
  | recordFieldPatterns ',' name ':' pattern { M.insert (strV $3) $5 $1 }


record :: { Exp }
  : '{' recordFields '}' { Record { epos = tokenToPos $1, etype = Nothing, erfields = $2 } }

recordFields :: { M.Map Name Exp }
  : name ':' exp                  { M.fromList [(strV $1, $3)] }
  | recordFields ',' name ':' exp { M.insert (strV $3) $5 $1 }


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
  | {- empty -}       { [] }

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

{
buildAbs :: Pos -> [Name] -> Exp -> Exp
buildAbs pos [param] body = Abs { epos = pos, etype = Nothing, eparam = param, ebody = body }
buildAbs pos (x:xs) body  = Abs { epos = pos, etype = Nothing, eparam = x, ebody = buildAbs pos xs body }

buildApp :: Pos -> Exp -> [Exp] -> Exp
buildApp pos f [arg]  = App { epos = pos, etype = Nothing, eabs = f, earg = arg, efieldAccess = False }
buildApp pos f xs = App { epos = pos, etype = Nothing, eabs = buildApp pos f (init xs) , earg = last xs, efieldAccess = False }

nameToPattern :: String -> Pattern
nameToPattern n | n == "_"           = PAny
                | n == "String"      = PCon n
                | n == "Bool"        = PCon n
                | n == "Num"         = PCon n
                | (isUpper . head) n = PCtor n []
                | otherwise          = PVar n

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
data ADT =
  ADT
    { adtname :: Name
    , adtparams :: [Name]
    , adtconstructors :: [ADTConstructor]
    }
    deriving(Eq, Show)

-- TODO: Add Pos
data ADTConstructor
  = ADTConstructor       { adtcname :: Name, adtcargs :: Maybe [TypeRef] }
  deriving(Eq, Show)

-- TODO: Rename
data TypeRef
  = TRSingle Name
  | TRComp Name [TypeRef]
  | TRArr TypeRef TypeRef
  | TRRecord (M.Map Name TypeRef)
  deriving(Eq, Show)

data Case =
  Case
    { casepos :: Pos
    , casetype :: Maybe Type
    , casepattern :: Pattern
    , caseexp :: Exp
    }
    deriving(Eq, Show)

data Pattern
  = PVar Name
  | PAny
  | PCtor Name [Pattern]
  | PNum String
  | PStr String
  | PBool String
  | PCon Name
  | PUserDef Name
  | PRecord (M.Map Name Pattern)
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
         | ListConstructor { epos :: Pos, etype :: Maybe Type, eelems :: [Exp] }
         | Record          { epos :: Pos, etype :: Maybe Type, erfields :: M.Map Name Exp }
         | Switch          { epos :: Pos, etype :: Maybe Type, ecases :: [Case], eexp :: Exp }
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
