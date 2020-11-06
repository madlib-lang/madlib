{
module Parse.Grammar where

import           Text.Printf
import           Control.Monad.Except
import qualified Data.Map             as M
import           Data.Char(isUpper)

import           Parse.Lexer
import           Infer.Type
import qualified AST.Source           as Src
import           Explain.Location
import           Explain.Meta
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
  'where' { Token _ TokenWhere }
  'is'   { Token _ TokenIs }
  '==='    { Token _ TokenTripleEq }
  false    { Token _ (TokenBool _) }
  true     { Token _ (TokenBool _) }
  'import' { Token _ TokenImport }
  'export' { Token _ TokenExport }
  'from'   { Token _ TokenFrom }
  '|'      { Token _ TokenPipe }
  '|>'     { Token _ TokenPipeOperator }
  '...'    { Token _ TokenSpreadOperator }
  'data'   { Token _ TokenData }

%left '==='
%left '+' '-'
%left '*' '/'
%right ','
%left '->' '|' '|>' 'ret'
%nonassoc '(' ')'
%nonassoc '=' '=>' '::' ':'
%%

ast :: { Src.AST }
  : adt ast          %shift { $2 { Src.aadts =  [$1] <> Src.aadts $2 } }
  | adt              %shift { Src.AST { Src.aimports = [], Src.aexps = [], Src.aadts = [$1], Src.apath = Nothing } }
  | exp ast          %shift { $2 { Src.aexps = [$1] <> Src.aexps $2 } }
  | exp              %shift { Src.AST { Src.aimports = [], Src.aexps = [$1], Src.aadts = [], Src.apath = Nothing } }
  | importDecls ast  %shift { $2 { Src.aimports = $1, Src.apath = Nothing } }
  | {- empty -}      %shift { Src.AST { Src.aimports = [], Src.aexps = [], Src.aadts = [], Src.apath = Nothing } }
  | rRet              { Src.AST { Src.aimports = [], Src.aexps = [], Src.aadts = [], Src.apath = Nothing } }
  | rRet ast          { $2 }
  | ast rRet          { $1 }
  | 'export' name '=' exp ast %shift { $5 { Src.aexps = (Meta emptyInfos (tokenToArea $1) (Src.Export (Meta emptyInfos (tokenToArea $2) (Src.Assignment (strV $2) $4)))) : Src.aexps $5 } }
  | name '::' typings maybeRet 'export' name '=' exp ast
      { $9 { Src.aexps = Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $8)) (Src.TypedExp (Meta emptyInfos (tokenToArea $1) (Src.Export (Meta emptyInfos (tokenToArea $2) (Src.Assignment (strV $6) $8)))) $3) : Src.aexps $9 } }

importDecls :: { [Src.Import] }
  : importDecl importDecls { $1:$2 }
  | importDecl             { [$1] }
  
importDecl :: { Src.Import }
  : 'import' '{' importNames '}' 'from' str rRet { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $6)) (Src.NamedImport $3 (strV $6)) }
  | 'import' name 'from' str rRet                { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Src.DefaultImport (strV $2) (strV $4)) }

importNames :: { [Src.Name] }
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


adt :: { Src.ADT }
  : 'data' name adtParameters rEq adtConstructors %shift { Src.ADT { Src.adtname = strV $2, Src.adtparams = $3, Src.adtconstructors = $5 } }

adtParameters :: { [Src.Name] }
  : name adtParameters %shift { strV $1 : $2 }
  | name               %shift { [strV $1] }
  | {- empty -}               { [] }

adtConstructors :: { [Src.ADTConstructor] }
  : adtConstructor rPipe adtConstructors      %shift { $1:$3 }
  | adtConstructor rRet                       %shift { [$1] }

adtConstructor :: { Src.ADTConstructor }
  : name adtConstructorArgs %shift { Src.ADTConstructor { Src.adtcname = strV $1, Src.adtcargs = Just $2 } }
  | name                    %shift { Src.ADTConstructor { Src.adtcname = strV $1, Src.adtcargs = Nothing } }

adtConstructorArgs :: { [Src.Typing] }
  : typing                    { [$1] }
  | adtConstructorArgs typing { $1 <> [$2] }

typings :: { Src.Typing }
  : typing '->' typings          { Meta emptyInfos (mergeAreas (getArea $1) (getArea $3)) (Src.TRArr $1 $3) }
  | compositeTyping '->' typings { Meta emptyInfos (mergeAreas (getArea $1) (getArea $3)) (Src.TRArr $1 $3) }
  | compositeTyping              { $1 }
  | typing                       { $1 }

typing :: { Src.Typing }
  : name                       { Meta emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1) }
  | '(' compositeTyping ')'    { $2 }
  | '(' typing '->' typing ')' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRArr $2 $4) }
  | '{' recordTypingArgs '}'   { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRRecord $2) }

compositeTyping :: { Src.Typing }
  : name compositeTypingArgs { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea (last $2))) (Src.TRComp (strV $1) $2) }

compositeTypingArgs :: { [Src.Typing] }
  : name                     { [Meta emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)] }
  | name compositeTypingArgs { (Meta emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)) : $2 }

recordTypingArgs :: { M.Map Src.Name Src.Typing }
  : name '::' typing                      { M.fromList [(strV $1, $3)] }
  | recordTypingArgs ',' name '::' typing { M.insert (strV $3) $5 $1 }

exp :: { Src.Exp }
  : literal                          { $1 }
  | record                           { $1 }
  | where                           { $1 }
  | operation                        { $1 }
  | listConstructor          %shift  { $1 }
  | typedExp                 %shift  { $1 }
  | js                       %shift  { Meta emptyInfos (tokenToArea $1) (Src.JSExp $ strV $1) }
  | name '=' exp             %shift  { Meta emptyInfos (tokenToArea $1) (Src.Assignment (strV $1) $3) }
  | name                     %shift  { Meta emptyInfos (tokenToArea $1) (Src.Var $ strV $1) }
  | name rParenL args ')'    %shift  { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Meta emptyInfos (tokenToArea $1) (Src.Var $ strV $1)) $3 }
  | exp '(' args ')'         %shift  { buildApp (mergeAreas (getArea $1) (tokenToArea $4)) $1 $3 }
  | '(' exp ')' '(' args ')' %shift  { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $6)) $2 $5 }
  | '(' params ')' '=>' exp  %shift  { buildAbs (mergeAreas (tokenToArea $1) (getArea $5)) $2 $5 }
  | '(' exp ')'              %shift  { $2 }
  | exp '.' name                     { Meta emptyInfos (mergeAreas (getArea $1) (tokenToArea $3)) (Src.FieldAccess $1 (Meta emptyInfos (tokenToArea $3) (Src.Var $ "." <> strV $3))) }
  | exp '.' name '(' args ')' %shift { buildApp (getArea $1) (Meta emptyInfos (getArea $1) (Src.FieldAccess $1 (Meta emptyInfos (tokenToArea $3) (Src.Var $ "." <> strV $3)))) $5 }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}'
      { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $17)) (Src.If $3 $7 $15) }

typedExp :: { Src.Exp }
  : exp '::' typings maybeRet %shift { Meta emptyInfos (mergeAreas (getArea $1) (getArea $3)) (Src.TypedExp $1 $3) }
  | name '::' typings maybeRet %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $3)) (Src.TypedExp (Meta emptyInfos (tokenToArea $1) (Src.Var (strV $1))) $3) }
  -- That grammar won't work well, we need to split the two parts ( before and after the 'ret', and join them during canonicalization )
  | name '::' typings 'ret' name '=' exp 
      { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $7)) (Src.TypedExp (Meta emptyInfos (mergeAreas (tokenToArea $5) (getArea $7)) (Src.Assignment (strV $5) $7)) $3) }

where :: { Src.Exp }
  : 'where' '(' exp ')' '{' maybeRet iss maybeRet '}' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $9)) (Src.Where $3 $7) }

iss :: { [Src.Is] }
  : 'is' pattern ':' exp             { [Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $4)) (Src.Is $2 $4)] }
  | iss 'ret' 'is' pattern ':' exp { $1 <> [Meta emptyInfos (mergeAreas (tokenToArea $3) (getArea $6)) (Src.Is $4 $6)] }

pattern :: { Src.Pattern }
  : nonCompositePattern { $1 }
  | compositePattern    { $1 }

nonCompositePattern :: { Src.Pattern }
  : name             { nameToPattern (tokenToArea $1) (strV $1) }
  | int              { Meta emptyInfos (tokenToArea $1) (Src.PNum $ strV $1) }
  | str              { Meta emptyInfos (tokenToArea $1) (Src.PStr $ strV $1) }
  | true             { Meta emptyInfos (tokenToArea $1) (Src.PBool $ strV $1) }
  | false            { Meta emptyInfos (tokenToArea $1) (Src.PBool $ strV $1) }
  | recordPattern    { $1 }
  | '(' pattern ')'  { $2 }


compositePattern :: { Src.Pattern }
  : name patterns %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea (last $2))) (Src.PCtor (strV $1) $2) }

patterns :: { [Src.Pattern] }
  : nonCompositePattern          { [$1] }
  | patterns nonCompositePattern { $1 <> [$2] }

recordPattern :: { Src.Pattern }
  : '{' recordFieldPatterns '}' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PRecord $2) }

recordFieldPatterns :: { M.Map Src.Name Src.Pattern }
  : name ':' pattern { M.fromList [(strV $1, $3)] }
  | recordFieldPatterns ',' name ':' pattern { M.insert (strV $3) $5 $1 }


record :: { Src.Exp }
  : '{' recordFields '}' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.Record $2) }

recordFields :: { [Src.Field] }
  : name ':' exp                  { [Src.Field (strV $1, $3)] }
  | '...' exp                     { [Src.Spread $2] }
  | recordFields ',' name ':' exp { $1 <> [Src.Field (strV $3, $5)] }
  | recordFields ',' '...' exp    { $1 <> [Src.Spread $4] }


operation :: { Src.Exp }
  : exp '+' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "+")) 
                         $1))) 
                      $3)
                 }
  | exp '-' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "-")) 
                         $1))) 
                      $3)
                 }
  | exp '*' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "*")) 
                         $1))) 
                      $3)
                 }
  | exp '/' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "/")) 
                         $1))) 
                      $3)
                 }
  | exp '===' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "===")) 
                         $1))) 
                      $3)
                   }
  | exp '|>' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "|>")) 
                         $1))) 
                      $3)
                  
                  }

listConstructor :: { Src.Exp }
  : '[' listItems ']' { Meta emptyInfos (tokenToArea $1) (Src.ListConstructor $2) }

listItems :: { [Src.Exp] }
  : exp               { [$1] }
  | exp ',' listItems { $1 : $3 }
  | {- empty -}       { [] }

literal :: { Src.Exp }
  : int                       { Meta emptyInfos (tokenToArea $1) (Src.LInt $ strV $1) }
  | str                       { Meta emptyInfos (tokenToArea $1) (Src.LStr $ strV $1) }
  | true                      { Meta emptyInfos (tokenToArea $1) (Src.LBool $ strV $1) }
  | false                     { Meta emptyInfos (tokenToArea $1) (Src.LBool $ strV $1) }

args :: { [Src.Exp] }
  : exp rComa args %shift { $1:$3 }
  | exp maybeRet   %shift { [$1] }

params :: { [Src.Name] }
  : name ',' params %shift { strV $1 : $3 }
  | name                   { [strV $1] }

{
buildAbs :: Area -> [Src.Name] -> Src.Exp -> Src.Exp
buildAbs area params body = buildAbs' (length params) area params body

-- TODO: use that nth to add context to params
-- To do this we need to somehow extend the Name with a version that is in a Meta
buildAbs' :: Int -> Area -> [Src.Name] -> Src.Exp -> Src.Exp
buildAbs' nth area [param] body = Meta emptyInfos area (Src.Abs param body)
buildAbs' nth area (x:xs) body  = Meta emptyInfos area (Src.Abs x (buildAbs' (nth + 1) area xs body))



buildApp :: Area -> Src.Exp -> [Src.Exp] -> Src.Exp
buildApp area f args = buildApp' (length args) area f args

-- TODO: use that nth to add context to args
buildApp' :: Int -> Area -> Src.Exp -> [Src.Exp] -> Src.Exp
buildApp' nth area f@(Meta _ _ f') [(Meta emptyInfos area' arg)]  = Meta emptyInfos area (Src.App f (Meta Infos { origin = Just f', nthArg = Just nth } area' arg))
buildApp' nth area f@(Meta _ _ f') xs     = 
  let (Meta emptyInfos area arg) = last xs
      argWithMeta        = Meta Infos { origin = Just f', nthArg = Just nth } area arg
  in  Meta emptyInfos area (Src.App (buildApp' (nth - 1) area f (init xs)) argWithMeta)



mergeAreas :: Area -> Area -> Area
mergeAreas (Area l _) (Area _ r) = Area l r


nameToPattern :: Area -> String -> Src.Pattern
nameToPattern area n | n == "_"      = Meta emptyInfos area Src.PAny
                | n == "String"      = Meta emptyInfos area (Src.PCon n)
                | n == "Bool"        = Meta emptyInfos area (Src.PCon n)
                | n == "Num"         = Meta emptyInfos area (Src.PCon n)
                | (isUpper . head) n = Meta emptyInfos area (Src.PCtor n [])
                | otherwise          = Meta emptyInfos area (Src.PVar n)


lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap f = alexMonadScan >>= f

parseError :: Token -> Alex a
parseError (Token (Area (Loc a l c) _) cls) =
  alexError (printf "Syntax error - line: %d, column: %d\nThe following token is not valid: %s" l c (show cls))

parse :: String -> Either String Src.AST
parse s = runAlex s parseMadlib
}
