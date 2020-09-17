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
  const    { Token _ TokenConst }
  int      { Token _ (TokenInt _) }
  str      { Token _ (TokenStr _) }
  name     { Token _ (TokenName _) }
  '='      { Token _ TokenEq }
  '+'      { Token _ TokenPlus }
  '::'     { Token _ TokenDoubleColon }
  '->'     { Token _ TokenArrow }
  '=>'     { Token _ TokenFatArrow }
  if       { Token _ TokenIf }
  ','      { Token _ TokenComa }
  '{'      { Token _ TokenLeftCurly }
  '}'      { Token _ TokenRightCurly }
  '('      { Token _ TokenLeftParen }
  ')'      { Token _ TokenRightParen }
  '==='    { Token _ TokenTripleEq }
  false    { Token _ (TokenBool _) }
  true     { Token _ (TokenBool _) }
  'import' { Token _ TokenImport }
%%

ast :: { AST }
  : importDefs functionDefs  { AST { aimports = $1, afunctions = $2, apath = Nothing } }
  | functionDefs             { AST { aimports = [], afunctions = $1, apath = Nothing } }

importDefs :: { [ImportDecl] }
  : importDef importDefs { $1:$2 }
  | importDef            { [$1] }
  
importDef :: { ImportDecl }
  : 'import' str { ImportDecl { ipos = tokenToPos $1, ipath = strV $2 } }

functionDefs :: { [FunctionDef] }
  : functionDef functionDefs { $1:$2 }
  | functionDef              { [$1] }

functionDef :: { FunctionDef }
-- Until we have inference, should we disallow that one ?
  : name '=' '(' params ')' '=>' body         { FunctionDef
                                                  { ftype = Nothing
                                                  , ftypeDef = Nothing -- map (const "*") $4 ??
                                                  , fpos = tokenToPos $1
                                                  , fname = strV $1
                                                  , fparams = $4
                                                  , fbody = $7 
                                                  }
                                              }
  | typing name '=' '(' params ')' '=>' body  { FunctionDef 
                                                  { ftype = Nothing
                                                  , ftypeDef = Just $1
                                                  , fpos = tpos $1
                                                  , fname = strV $2
                                                  , fparams = $5
                                                  , fbody = $8 
                                                  }
                                              }

-- TODO: Make it a TypeDecl ?
typing :: { Typing }
  : name '::' types { Typing { tpos = tokenToPos $1, tfor = strV $1, ttypes = $3 } }

types :: { [Type] }
  : name '->' types { (strV $1) : $3 }
  | name            { [(strV $1)] }

exps :: { [Exp] }
  : exp exps            { $1:$2 }
  | exp                 { [$1] }

exp :: { Exp }
  : int               { IntLit       { etype = Just "Num", epos = tokenToPos $1} }
  | str               { StringLit    { etype = Just "String", epos = tokenToPos $1} }
  | false             { BoolLit      { etype = Just "Bool", epos = tokenToPos $1} }
  | true              { BoolLit      { etype = Just "Bool", epos = tokenToPos $1} }
  | exp operator exp  { Operation    { etype = Nothing, epos = epos $1, eleft = $1, eoperator = $2, eright = $3 }}
  | name              { VarAccess    { etype = Nothing, epos = tokenToPos $1, ename = strV $1 }}
  | name '(' args ')' { FunctionCall { etype = Nothing, epos = tokenToPos $1, ename = strV $1, eargs = $3 }}

args :: { [Exp] }
  : exp          { [$1] }
  | exp ',' args { $1:$3 }

params :: { [Param] }
  : name ',' params { (strV $1) : $3 }
  | name            { [(strV $1)] }

body :: { Body }
  : exp { Body $1 }

operator :: { Operator }
  : '===' { TripleEq }
  | '+'   { Plus }

{

data AST =
  AST
    { aimports   :: [ImportDecl]
    , afunctions :: [FunctionDef]
    , apath      :: Maybe Path
    }
    deriving(Eq, Show)

data ImportDecl =
  ImportDecl
    { ipos  :: Pos
    , ipath :: Path
    }
    deriving(Eq, Show)

data FunctionDef =
  FunctionDef
    { ftype    :: Maybe Type
    , ftypeDef :: Maybe Typing
    , fpos     :: Pos
    , fname    :: String
    , fparams  :: [Param]
    , fbody    :: Body
    }
    deriving(Eq, Show)

data Typing =
  Typing
    { tpos   :: Pos
    , tfor   :: Name
    , ttypes :: [Type]
    }
    deriving(Eq, Show)

data Exp = IntLit       { etype :: Maybe Type, epos :: Pos }
         | StringLit    { etype :: Maybe Type, epos :: Pos }
         | BoolLit      { etype :: Maybe Type, epos :: Pos }
         | Operation    { etype :: Maybe Type, epos :: Pos, eleft :: Exp, eoperator :: Operator, eright :: Exp }
         | VarAccess    { etype :: Maybe Type, epos :: Pos, ename :: Name }
         | FunctionCall { etype :: Maybe Type, epos :: Pos, ename :: Name, eargs :: [Exp] }
         deriving(Eq, Show)

type Path  = String
type Type  = String
type Param = String
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
