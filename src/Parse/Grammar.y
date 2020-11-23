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
  number   { Token _ (TokenNumber _) }
  str      { Token _ (TokenStr _) }
  name     { Token _ (TokenName _) }
  js       { Token _ (TokenJSBlock _) }
  'ret'    { Token _ TokenReturn }
  '='      { Token _ TokenEq }
  '+'      { Token _ TokenPlus }
  '++'     { Token _ TokenDoublePlus }
  '-'      { Token _ TokenDash }
  '*'      { Token _ TokenStar }
  '/'      { Token _ TokenSlash }
  '%'      { Token _ TokenPercent }
  '::'     { Token _ TokenDoubleColon }
  ':'      { Token _ TokenColon }
  '?'      { Token _ TokenQuestionMark }
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
  'where'  { Token _ TokenWhere }
  'is'     { Token _ TokenIs }
  '=='     { Token _ TokenDoubleEq }
  false    { Token _ (TokenBool _) }
  true     { Token _ (TokenBool _) }
  'import' { Token _ TokenImport }
  'export' { Token _ TokenExport }
  'from'   { Token _ TokenFrom }
  '|'      { Token _ TokenPipe }
  '|>'     { Token _ TokenPipeOperator }
  '...'    { Token _ TokenSpreadOperator }
  'data'   { Token _ TokenData }
  '&&'     { Token _ TokenDoubleAmpersand }
  '||'     { Token _ TokenDoublePipe }
  '>'      { Token _ TokenRightChevron }
  '<'      { Token _ TokenLeftChevron }
  'tuple>' { Token _ TokenTupleEnd }
  '>='     { Token _ TokenRightChevronEq }
  '<='     { Token _ TokenLeftChevronEq }
  '!'      { Token _ TokenExclamationMark }


%nonassoc LOWEST
%left '?' '->' '|' where is 'if' '='
%left ':' 'else'
%left '|>'
%left '>' '<' '>=' '<=' '=='
%left '+' '-' '||'
%left '*' '/' '%' '&&'
%left ','
%nonassoc '(' ')' 'tuple>' '=>' '::' where is 'ret' '{' '}' '[' ']'
%right '!'
%nonassoc HIGHEST
%%

ast :: { Src.AST }
  : adt ast          %shift { $2 { Src.aadts =  [$1] <> Src.aadts $2 } }
  | exp ast          %shift { $2 { Src.aexps = [$1] <> Src.aexps $2 } }
  | importDecls ast  %shift { $2 { Src.aimports = $1, Src.apath = Nothing } }
  | {- empty -}      %shift { Src.AST { Src.aimports = [], Src.aexps = [], Src.aadts = [], Src.apath = Nothing } }
  | 'ret'            %shift { Src.AST { Src.aimports = [], Src.aexps = [], Src.aadts = [], Src.apath = Nothing } }
  | 'ret' ast        %shift { $2 }
  | 'export' name '=' exp ast %shift { $5 { Src.aexps = (Meta emptyInfos (tokenToArea $1) (Src.Export (Meta emptyInfos (tokenToArea $2) (Src.Assignment (strV $2) $4)))) : Src.aexps $5 } }
  | name '::' typings maybeRet 'export' name '=' exp ast
      { $9 { Src.aexps = Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $8)) (Src.TypedExp (Meta emptyInfos (tokenToArea $1) (Src.Export (Meta emptyInfos (tokenToArea $2) (Src.Assignment (strV $6) $8)))) $3) : Src.aexps $9 } }

importDecls :: { [Src.Import] }
  : importDecl importDecls %shift { $1:$2 }
  | importDecl             %shift { [$1] }
  
importDecl :: { Src.Import }
  : 'import' '{' importNames '}' 'from' str 'ret' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $6)) (Src.NamedImport $3 (strV $6) (strV $6)) }
  | 'import' name 'from' str 'ret'                { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Src.DefaultImport (strV $2) (strV $4) (strV $4)) }

importNames :: { [Src.Name] }
  : importNames ',' name %shift { $1 <> [strV $3] }
  | name                 %shift { [strV $1] }


rets :: { [TokenClass] }
  : 'ret'       %shift{ [] }
  | rets 'ret'  %shift{ [] }
  | {- empty -} %shift { [] }

maybeRet :: { [TokenClass] }
  : 'ret'       { [] }
  | {- empty -} { [] }

rEq :: { [TokenClass] }
  : '='       { [] }
  | 'ret' '=' { [] }

rPipe :: { [TokenClass] }
  : '|'       { [] }
  | 'ret' '|' { [] }

rComa :: { [TokenClass] }
  : ','       { [] }
  | ',' 'ret' { [] }
  | 'ret' ',' { [] }


adt :: { Src.ADT }
  : 'data' name adtParameters rEq adtConstructors          %shift { Src.ADT { Src.adtname = strV $2, Src.adtparams = $3, Src.adtconstructors = $5, Src.adtexported = False } }
  | 'export' 'data' name adtParameters rEq adtConstructors %shift { Src.ADT { Src.adtname = strV $3, Src.adtparams = $4, Src.adtconstructors = $6, Src.adtexported = True } }

adtParameters :: { [Src.Name] }
  : name adtParameters %shift { strV $1 : $2 }
  | {- empty -}               { [] }

adtConstructors :: { [Src.Constructor] }
  : adtConstructor rPipe adtConstructors      %shift { $1:$3 }
  | adtConstructor 'ret'                      %shift { [$1] }
  | adtConstructor                            %shift { [$1] }

adtConstructor :: { Src.Constructor }
  : name adtConstructorArgs %shift { Src.Constructor (strV $1) $2 }
  | name                    %shift { Src.Constructor (strV $1) [] }

adtConstructorArgs :: { [Src.Typing] }
  : typing                     %shift { [$1] }
  | name '.' name              %shift { [Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $1)) (Src.TRComp (strV $1<>"."<>strV $3) [])] }
  | '(' compositeTyping ')'    %shift { [$2] }
  | '(' adtConstructorArgs ')' %shift { $2 }
  | adtConstructorArgs typing  %shift { $1 <> [$2] }

typings :: { Src.Typing }
  : typing '->' typings          %shift { Meta emptyInfos (mergeAreas (getArea $1) (getArea $3)) (Src.TRArr $1 $3) }
  | compositeTyping '->' typings %shift { Meta emptyInfos (mergeAreas (getArea $1) (getArea $3)) (Src.TRArr $1 $3) }
  | compositeTyping              %shift { $1 }
  | typing                       %shift { $1 }

typing :: { Src.Typing }
  : name                        %shift { Meta emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1) }
  | '(' typing '->' typings ')' %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRArr $2 $4) }
  | typing '->' typings         %shift { Meta emptyInfos (mergeAreas (getArea $1) (getArea $3)) (Src.TRArr $1 $3) }
  | '{' recordTypingArgs '}'    %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRRecord $2) }
  | '<' tupleTypings 'tuple>'   %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRTuple $2) }

compositeTyping :: { Src.Typing }
  : name compositeTypingArgs          { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea (last $2))) (Src.TRComp (strV $1) $2) }
  | name '.' name compositeTypingArgs { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea (last $4))) (Src.TRComp (strV $1<>"."<>strV $3) $4) }
  | name '.' name                     { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $1)) (Src.TRComp (strV $1<>"."<>strV $3) []) }

compositeTypingArgs :: { [Src.Typing] }
  : name                        { [Meta emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)] }
  | name compositeTypingArgs    { (Meta emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)) : $2 }
  | typings                     { [$1] }
  | '(' typings ')'             { [$2] }

recordTypingArgs :: { M.Map Src.Name Src.Typing }
  : name '::' typing                               { M.fromList [(strV $1, $3)] }
  | name '::' compositeTyping                      { M.fromList [(strV $1, $3)] }
  | recordTypingArgs ',' name '::' typing          { M.insert (strV $3) $5 $1 }
  | recordTypingArgs ',' name '::' compositeTyping { M.insert (strV $3) $5 $1 }

tupleTypings :: { [Src.Typing] }
  : typing ',' typing                   { [$1, $3] }
  | typing ',' compositeTyping          { [$1, $3] }
  | compositeTyping ',' typing          { [$1, $3] }
  | compositeTyping ',' compositeTyping { [$1, $3] }
  | tupleTypings ',' typing             { $1 <> [$3] }
  | tupleTypings ',' compositeTyping    { $1 <> [$3] }

exp :: { Src.Exp }
  : literal                          { $1 }
  | record                    %shift { $1 }
  | where                     %shift { $1 }
  | tupleConstructor          %shift { $1 }
  | operation                        { $1 }
  | listConstructor           %shift { $1 }
  | typedExp                  %shift { $1 }
  | js                        %shift { Meta emptyInfos (tokenToArea $1) (Src.JSExp $ strV $1) }
  | name '=' exp              %shift { Meta emptyInfos (tokenToArea $1) (Src.Assignment (strV $1) $3) }
  | name                      %shift { Meta emptyInfos (tokenToArea $1) (Src.Var $ strV $1) }
  | name '(' args ')'         %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Meta emptyInfos (tokenToArea $1) (Src.Var $ strV $1)) $3 }
  | name '(' 'ret' args ')'   %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Meta emptyInfos (tokenToArea $1) (Src.Var $ strV $1)) $4 }
  | exp '(' args ')'          %shift { buildApp (mergeAreas (getArea $1) (tokenToArea $4)) $1 $3 }
  | '(' exp ')' '(' args ')'  %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $6)) $2 $5 }
  | '(' params ')' '=>' '(' rets exp rets ')'  %shift { buildAbs (mergeAreas (tokenToArea $1) (tokenToArea $9)) $2 $7 }
  | '(' exp ')'               %shift { $2 }
  | exp '.' name              %shift { Meta emptyInfos (mergeAreas (getArea $1) (tokenToArea $3)) (Src.FieldAccess $1 (Meta emptyInfos (tokenToArea $3) (Src.Var $ "." <> strV $3))) }
  | exp '.' name '(' args ')' %shift { buildApp (getArea $1) (Meta emptyInfos (getArea $1) (Src.FieldAccess $1 (Meta emptyInfos (tokenToArea $3) (Src.Var $ "." <> strV $3)))) $5 }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}'
      { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $17)) (Src.If $3 $7 $15) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet exp
      { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $10)) (Src.If $3 $6 $10) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet exp 'ret'
      { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $10)) (Src.If $3 $6 $10) }
  | exp '?' maybeRet exp maybeRet ':' maybeRet exp
      { Meta emptyInfos (mergeAreas (getArea $1) (getArea $8)) (Src.If $1 $4 $8) }
  | exp '?' maybeRet exp maybeRet ':' maybeRet exp 'ret'
      { Meta emptyInfos (mergeAreas (getArea $1) (getArea $8)) (Src.If $1 $4 $8) }

typedExp :: { Src.Exp }
  : '(' exp '::' typings ')'  %shift { Meta emptyInfos (mergeAreas (getArea $2) (getArea $4)) (Src.TypedExp $2 $4) }
  | '(' name '::' typings ')' %shift { Meta emptyInfos (mergeAreas (tokenToArea $2) (getArea $4)) (Src.TypedExp (Meta emptyInfos (tokenToArea $2) (Src.Var (strV $2))) $4) }
  | name '::' typings 'ret' name '=' exp 
      %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $7)) (Src.TypedExp (Meta emptyInfos (mergeAreas (tokenToArea $5) (getArea $7)) (Src.Assignment (strV $5) $7)) $3) }

where :: { Src.Exp }
  : 'where' '(' exp ')' '{' maybeRet iss maybeRet '}' %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $9)) (Src.Where $3 $7) }
  | 'where' '(' exp ')' maybeRet iss                  %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $ last $6)) (Src.Where $3 $6) }
  | 'where' '(' exp ')' maybeRet iss 'ret'            %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $ last $6)) (Src.Where $3 $6) }

iss :: { [Src.Is] }
  : 'is' pattern ':' maybeRet exp                    %shift { [Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $5)) (Src.Is $2 $5)] }
  | 'is' pattern ':' maybeRet exp 'ret'              %shift { [Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $5)) (Src.Is $2 $5)] }
  | iss maybeRet 'is' pattern ':' maybeRet exp       %shift { $1 <> [Meta emptyInfos (mergeAreas (tokenToArea $3) (getArea $7)) (Src.Is $4 $7)] }
  | iss maybeRet 'is' pattern ':' maybeRet exp 'ret' %shift { $1 <> [Meta emptyInfos (mergeAreas (tokenToArea $3) (getArea $7)) (Src.Is $4 $7)] }

pattern :: { Src.Pattern }
  : nonCompositePattern %shift { $1 }
  | compositePattern    %shift { $1 }

nonCompositePattern :: { Src.Pattern }
  : name             { nameToPattern (tokenToArea $1) (strV $1) }
  | number           { Meta emptyInfos (tokenToArea $1) (Src.PNum $ strV $1) }
  | str              { Meta emptyInfos (tokenToArea $1) (Src.PStr $ strV $1) }
  | true             { Meta emptyInfos (tokenToArea $1) (Src.PBool $ strV $1) }
  | false            { Meta emptyInfos (tokenToArea $1) (Src.PBool $ strV $1) }
  | recordPattern    { $1 }
  | listPattern      { $1 }
  | tuplePattern     { $1 }
  | '(' pattern ')'  { $2 }


compositePattern :: { Src.Pattern }
  : name patterns          %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea (last $2))) (Src.PCtor (strV $1) $2) }
  | name '.' name patterns %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea (last $4))) (Src.PCtor (strV $1 <> "." <> strV $3) $4) }
  | name '.' name          %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PCtor (strV $1 <> "." <> strV $3) []) }

patterns :: { [Src.Pattern] }
  : nonCompositePattern          { [$1] }
  | patterns nonCompositePattern { $1 <> [$2] }

recordPattern :: { Src.Pattern }
  : '{' recordFieldPatterns '}' %shift { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PRecord $2) }

recordFieldPatterns :: { M.Map Src.Name Src.Pattern }
  : name ':' pattern                         { M.fromList [(strV $1, $3)] }
  | name                                     { M.fromList [(strV $1, Meta emptyInfos (tokenToArea $1) (Src.PVar (strV $1)))] }
  | recordFieldPatterns ',' spreadPattern    { M.insert "..." $3 $1 }
  | recordFieldPatterns ',' name ':' pattern { M.insert (strV $3) $5 $1 }
  | recordFieldPatterns ',' name             { M.insert (strV $3) (Meta emptyInfos (tokenToArea $3) (Src.PVar (strV $3))) $1 }

listPattern :: { Src.Pattern }
  : '[' listItemPatterns ']' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PList $2) }

listItemPatterns :: { [Src.Pattern] }
  : pattern                            { [$1] }
  | listItemPatterns ',' pattern       { $1 <> [$3] }
  | listItemPatterns ',' spreadPattern { $1 <> [$3] }
  | {- empty -}                        { [] }

spreadPattern :: { Src.Pattern }
  : '...' name  { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.PSpread (nameToPattern (tokenToArea $2) (strV $2))) }


tuplePattern :: { Src.Pattern }
  : '<' tupleItemPatterns 'tuple>' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PTuple $2) }

tupleItemPatterns :: { [Src.Pattern] }
  : pattern ',' pattern                %shift { [$1, $3] }
  | listItemPatterns ',' pattern       %shift { $1 <> [$3] }


record :: { Src.Exp }
  : '{' rets recordFields rets '}' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.Record $3) }

recordFields :: { [Src.Field] }
  : name ':' exp                            { [Src.Field (strV $1, $3)] }
  | '...' exp                               { [Src.FieldSpread $2] }
  | recordFields ',' name ':' exp           { $1 <> [Src.Field (strV $3, $5)] }
  | recordFields rets ',' rets name ':' exp { $1 <> [Src.Field (strV $5, $7)] }
  | recordFields ',' '...' exp              { $1 <> [Src.FieldSpread $4] }
  | {- empty -}                             { [] }


operation :: { Src.Exp }
  : exp '+' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "+")) 
                         $1))) 
                      $3)
                 }
  | exp '++' exp { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "++")) 
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
  | exp '%' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "%"))
                         $1)))
                      $3)
                 }
  | exp '==' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "==")) 
                         $1))) 
                      $3)
                   }
  | exp '&&' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "&&")) 
                         $1))) 
                      $3)
                   }
  | exp '||' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "||")) 
                         $1))) 
                      $3)
                   }
  | exp '>' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var ">")) 
                         $1))) 
                      $3)
                   }
  | exp '<' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "<")) 
                         $1))) 
                      $3)
                   }
  | exp '>=' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var ">=")) 
                         $1))) 
                      $3)
                   }
  | exp '<=' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "<=")) 
                         $1))) 
                      $3)
                   }
  | '!' exp  { Meta emptyInfos (mergeAreas (tokenToArea $1) (getArea $2)) (Src.App (Meta emptyInfos (tokenToArea $1) (Src.Var "!")) $2) }
  | exp '|>' exp  { Meta emptyInfos (getArea $1) (Src.App
                      ((Meta emptyInfos (getArea $1) (Src.App
                         (Meta emptyInfos (tokenToArea $2) (Src.Var "|>")) 
                         $1))) 
                      $3)
                  
                  }

listConstructor :: { Src.Exp }
  : '[' rets listItems rets ']' { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.ListConstructor $3) }

listItems :: { [Src.ListItem] }
  : exp                         { [Src.ListItem $1] }
  | listItems ',' exp           { $1 <> [Src.ListItem $3] }
  | listItems rets ',' rets exp { $1 <> [Src.ListItem $5] }
  | '...' exp                   { [Src.ListSpread $2] }
  | listItems ',' '...' exp     { $1 <> [Src.ListSpread $4] }
  | {- empty -}                 { [] }

tupleConstructor :: { Src.Exp }
  : '<' tupleItems 'tuple>'        { Meta emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TupleConstructor $2) }

tupleItems :: { [Src.Exp] }
  : exp ',' exp                  %shift { [$1, $3] }
  | tupleItems ',' exp           %shift { $1 <> [$3] }


literal :: { Src.Exp }
  : number                    { Meta emptyInfos (tokenToArea $1) (Src.LNum $ strV $1) }
  | str                       { Meta emptyInfos (tokenToArea $1) (Src.LStr $ strV $1) }
  | true                      { Meta emptyInfos (tokenToArea $1) (Src.LBool $ strV $1) }
  | false                     { Meta emptyInfos (tokenToArea $1) (Src.LBool $ strV $1) }

args :: { [Src.Exp] }
  : exp rComa args %shift { $1:$3 }
  | exp            %shift { [$1] }
  | exp 'ret'      %shift { [$1] }

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
                | n == "Boolean"     = Meta emptyInfos area (Src.PCon n)
                | n == "Number"      = Meta emptyInfos area (Src.PCon n)
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
