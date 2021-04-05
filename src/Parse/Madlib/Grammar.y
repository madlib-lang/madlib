{
module Parse.Madlib.Grammar where

import           Text.Printf
import           Control.Monad.Except
import qualified Data.Map             as M
import           Data.Char(isUpper)

import           Parse.Madlib.Lexer
import           Infer.Type
import qualified AST.Source           as Src
import           Explain.Location
import           Explain.Meta
import           Target
import           Debug.Trace (trace)
import           Text.Show.Pretty (ppShow)
}

%name parseMadlib ast
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexerWrap } { Token _ TokenEOF }

%token
  number      { Token _ (TokenNumber _) }
  str         { Token _ (TokenStr _) }
  strTplStart { Token _ (TokenTemplateStringStart) }
  strTplEnd   { Token _ (TokenTemplateStringEnd _) }
  name        { Token _ (TokenName _) }
  nameC       { Token _ (TokenConstraintName _) }
  js          { Token _ (TokenJSBlock _) }
  'ret'       { Token _ TokenReturn }
  '='         { Token _ TokenEq }
  '+'         { Token _ TokenPlus }
  '++'        { Token _ TokenDoublePlus }
  '-'         { Token _ TokenDash }
  '*'         { Token _ TokenStar }
  '/'         { Token _ TokenSlash }
  '%'         { Token _ TokenPercent }
  '::'        { Token _ TokenDoubleColon }
  ':'         { Token _ TokenColon }
  '?'         { Token _ TokenQuestionMark }
  '->'        { Token _ TokenArrow }
  '=>'        { Token _ TokenFatArrow }
  '.'         { Token _ TokenDot }
  ','         { Token _ TokenComma }
  '('         { Token _ TokenLeftParen }
  ')'         { Token _ TokenRightParen }
  '{{'        { Token _ TokenLeftDoubleCurly }
  '{'         { Token _ TokenLeftCurly }
  '}'         { Token _ TokenRightCurly }
  '['         { Token _ TokenLeftSquaredBracket }
  ']'         { Token _ TokenRightSquaredBracket }
  '$'         { Token _ TokenDollar }
  'if'        { Token _ TokenIf }
  'else'      { Token _ TokenElse }
  'interface' { Token _ TokenInterface }
  'instance'  { Token _ TokenInstance }
  'where'     { Token _ TokenWhere }
  'is'        { Token _ TokenIs }
  'return'    { Token _ TokenReturnKeyword }
  '=='        { Token _ TokenDoubleEq }
  '!='        { Token _ TokenExclamationMarkEq }
  false       { Token _ (TokenBool _) }
  true        { Token _ (TokenBool _) }
  'import'    { Token _ TokenImport }
  'export'    { Token _ TokenExport }
  'from'      { Token _ TokenFrom }
  '|'         { Token _ TokenPipe }
  'pipe'      { Token _ TokenPipeKeyword }
  '|>'        { Token _ TokenPipeOperator }
  '...'       { Token _ TokenSpreadOperator }
  'type'      { Token _ TokenType }
  'alias'     { Token _ TokenAlias }
  '&&'        { Token _ TokenDoubleAmpersand }
  '||'        { Token _ TokenDoublePipe }
  '>'         { Token _ TokenRightChevron }
  '<'         { Token _ TokenLeftChevron }
  'jsx<'      { Token _ TokenJsxTagOpenStart }
  'jsx</'     { Token _ TokenJsxTagOpenEnd }
  'jsx<1'     { Token _ TokenJsxTagOpenSingle }
  'tuple>'    { Token _ TokenTupleEnd }
  '>='        { Token _ TokenRightChevronEq }
  '<='        { Token _ TokenLeftChevronEq }
  '!'         { Token _ TokenExclamationMark }

%nonassoc LOWEST
%left ':' '->' '|' where is 'else' '='
%left '?' 'if'
%left '|>'
%left '>' '<' '>=' '<=' '==' '!='
%left '+' '-' '||'
%left '*' '/' '%' '&&'
%left ','
%nonassoc '(' ')' 'tuple>' '=>' '::' where is 'ret' '{' '}' '[' ']'
%right '!'
%nonassoc HIGHEST
%%

ast :: { Src.AST }
  : typedecl ast     %shift { $2 { Src.atypedecls =  [$1] <> Src.atypedecls $2 } }
  | exp ast          %shift { $2 { Src.aexps = [$1] <> Src.aexps $2 } }
  | importDecls ast  %shift { $2 { Src.aimports = $1, Src.apath = Nothing } }
  | interface ast    %shift { $2 { Src.ainterfaces = [$1] <> (Src.ainterfaces $2), Src.apath = Nothing } }
  | instance ast     %shift { $2 { Src.ainstances = [$1] <> (Src.ainstances $2), Src.apath = Nothing } }
  | {- empty -}      %shift { Src.AST { Src.aimports = [], Src.aexps = [], Src.atypedecls = [], Src.ainterfaces = [], Src.ainstances = [], Src.apath = Nothing } }
  | 'ret'            %shift { Src.AST { Src.aimports = [], Src.aexps = [], Src.atypedecls = [], Src.ainterfaces = [], Src.ainstances = [], Src.apath = Nothing } }
  | 'ret' ast        %shift { $2 }
  | 'export' name '=' exp ast %shift { $5 { Src.aexps = (Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $4)) (Src.Export (Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $4)) (Src.Assignment (strV $2) $4)))) : Src.aexps $5 } }
  | name '::' constrainedTyping maybeRet 'export' name '=' exp ast
      { $9 { Src.aexps = Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $8)) (Src.TypedExp (Src.Source emptyInfos (mergeAreas (tokenToArea $5) (Src.getArea $8)) (Src.Export (Src.Source emptyInfos (mergeAreas (tokenToArea $6) (Src.getArea $8)) (Src.Assignment (strV $6) $8)))) $3) : Src.aexps $9 } }

importDecls :: { [Src.Import] }
  : importDecl importDecls %shift { $1:$2 }
  | importDecl             %shift { [$1] }
  
importDecl :: { Src.Import }
  : 'import' '{' importNames '}' 'from' str rets { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $6)) (Src.NamedImport $3 (sanitizeImportPath $ strV $6) (sanitizeImportPath $ strV $6)) }
  | 'import' name 'from' str rets                { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Src.DefaultImport (strV $2) (sanitizeImportPath $ strV $4) (sanitizeImportPath $ strV $4)) }

importNames :: { [Src.Name] }
  : importNames ',' name %shift { $1 <> [strV $3] }
  | name                 %shift { [strV $1] }
  | {- empty -}          %shift { [] }


interface :: { Src.Interface }
  : 'interface' name names '{' rets methodDefs rets '}'                          { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $8)) (Src.Interface [] (strV $2) $3 $6) }
  | 'interface' constraint '=>' name names '{' rets methodDefs rets '}'          { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $10)) (Src.Interface [$2] (strV $4) $5 $8) }
  | 'interface' '(' constraints ')' '=>' name names '{' rets methodDefs rets '}' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $12)) (Src.Interface $3 (strV $6) $7 $10) }


names :: { [Src.Name] }
  : name       { [strV $1] }
  | name names { (strV $1) : $2 }

methodDefs :: { M.Map Src.Name Src.Typing }
  : name '::' constrainedTyping                 { M.fromList [(strV $1, $3)] }
  | methodDefs rets name '::' constrainedTyping { M.union (M.fromList [(strV $3, $5)]) $1 }

instance :: { Src.Instance }
  : 'instance' name manyTypings '{{' rets methodImpls rets '}'                          %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $8)) (Src.Instance [] (strV $2) $3 $6) }
  | 'instance' instanceConstraint '=>' name manyTypings '{{' rets methodImpls rets '}'           { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $10)) (Src.Instance [$2] (strV $4) $5 $8) }
  | 'instance' '(' instanceConstraints ')' '=>' name manyTypings '{{' rets methodImpls rets '}' %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $12)) (Src.Instance $3 (strV $6) $7 $10) }

methodImpls :: { M.Map Src.Name Src.Exp }
  : name '=' exp { M.fromList [(strV $1, Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $3)) (Src.Assignment (strV $1) $3))] }
  | methodImpls rets name '=' exp { M.union (M.fromList [(strV $3, Src.Source emptyInfos (mergeAreas (tokenToArea $3) (Src.getArea $5)) (Src.Assignment (strV $3) $5))]) $1 }

rets :: { [TokenClass] }
  : 'ret'       %shift{ [] }
  | rets 'ret'  %shift{ [] }
  | {- empty -} %shift { [] }

maybeRet :: { [TokenClass] }
  : 'ret'       { [] }
  | {- empty -} { [] }

maybeComa :: { [TokenClass] }
  : ','       { [] }
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


typedecl :: { Src.TypeDecl }
  : 'type' name typeParams rEq adtConstructors          %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $ last $5)) Src.ADT { Src.adtname = strV $2, Src.adtparams = $3, Src.adtconstructors = $5, Src.adtexported = False } }
  | 'export' 'type' name typeParams rEq adtConstructors %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $ last $6)) Src.ADT { Src.adtname = strV $3, Src.adtparams = $4, Src.adtconstructors = $6, Src.adtexported = True } }
  | 'alias' name typeParams rEq typings                 %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $5)) Src.Alias { Src.aliasname = strV $2, Src.aliasparams = $3, Src.aliastype = $5, Src.aliasexported = False } }
  | 'export' 'alias' name typeParams rEq typings        %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $6)) Src.Alias { Src.aliasname = strV $3, Src.aliasparams = $4, Src.aliastype = $6, Src.aliasexported = True } }

typeParams :: { [Src.Name] }
  : name typeParams %shift { strV $1 : $2 }
  | {- empty -}               { [] }

adtConstructors :: { [Src.Constructor] }
  : adtConstructor rPipe adtConstructors      %shift { $1:$3 }
  | adtConstructor 'ret'                      %shift { [$1] }
  | adtConstructor                            %shift { [$1] }

adtConstructor :: { Src.Constructor }
  : name adtConstructorArgs %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $ last $2)) (Src.Constructor (strV $1) $2) }
  | name                    %shift { Src.Source emptyInfos (tokenToArea $1) (Src.Constructor (strV $1) []) }

adtConstructorArgs :: { [Src.Typing] }
  : typing                     %shift { [$1] }
  | name '.' name              %shift { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRComp (strV $1<>"."<>strV $3) [])] }
  | '(' compositeTyping ')'    %shift { [$2] }
  | '(' adtConstructorArgs ')' %shift { $2 }
  | adtConstructorArgs typing  %shift { $1 <> [$2] }


constrainedTyping :: { Src.Typing }
  : constraint '=>' typings      %shift { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.TRConstrained [$1] $3) }
  | '(' constraints ')' '=>' typings      %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $5)) (Src.TRConstrained $2 $5) }
  | typings  %shift { $1 }

instanceConstraints :: { [Src.Typing] }
  : instanceConstraint { [$1] }
  | instanceConstraints ',' instanceConstraint { $1 <> [$3] }

instanceConstraint :: { Src.Typing }
  : nameC nameC                 { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2))]) }
  | nameC nameC nameC            { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2)), Src.Source emptyInfos (tokenToArea $3) (Src.TRSingle (strV $3))]) }
  | nameC nameC nameC nameC       { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2)), Src.Source emptyInfos (tokenToArea $3) (Src.TRSingle (strV $3)), Src.Source emptyInfos (tokenToArea $4) (Src.TRSingle (strV $4))]) }
  | nameC nameC '(' typings ')' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2)), $4]) }
  | nameC '(' typings ')' nameC { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRComp (strV $1) [$3, Src.Source emptyInfos (tokenToArea $5) (Src.TRSingle (strV $5))]) }

constraints :: { [Src.Typing] }
  : constraint { [$1] }
  | constraints ',' constraint { $1 <> [$3] }

constraint :: { Src.Typing }
  : name name                 { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2))]) }
  | name name name            { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2)), Src.Source emptyInfos (tokenToArea $3) (Src.TRSingle (strV $3))]) }
  | name name name name       { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2)), Src.Source emptyInfos (tokenToArea $3) (Src.TRSingle (strV $3)), Src.Source emptyInfos (tokenToArea $4) (Src.TRSingle (strV $4))]) }
  | name name '(' typings ')' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRComp (strV $1) [Src.Source emptyInfos (tokenToArea $2) (Src.TRSingle (strV $2)), $4]) }
  | name '(' typings ')' name { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRComp (strV $1) [$3, Src.Source emptyInfos (tokenToArea $5) (Src.TRSingle (strV $5))]) }


simpleTypings :: { [Src.Typing] }
  : name               %shift { [Src.Source emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)] }
  | name simpleTypings %shift { [Src.Source emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)] <> $2 }

manyTypings :: { [Src.Typing] }
  : typing              { [$1] }
  | typing manyTypings  { $1:$2 }

typings :: { Src.Typing }
  : typings '->' typings         %shift { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.TRArr $1 $3) }
  | compositeTyping '->' typings %shift { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.TRArr $1 $3) }
  | compositeTyping              %shift { $1 }
  | typing                       %shift { $1 }

typing :: { Src.Typing }
  : name                        %shift { Src.Source emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1) }
  | '(' ')'                     %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.TRSingle "()") }
  | '(' typings ')'             %shift { $2 }
  | '{' recordTypingArgs '}'    %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRRecord $2) }
  | '<' tupleTypings 'tuple>'   %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRTuple $2) }

compositeTyping :: { Src.Typing }
  : name compositeTypingArgs          { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea (last $2))) (Src.TRComp (strV $1) $2) }
  | name '.' name compositeTypingArgs { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea (last $4))) (Src.TRComp (strV $1<>"."<>strV $3) $4) }
  | name '.' name                     { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRComp (strV $1<>"."<>strV $3) []) }

compositeTypingArgs :: { [Src.Typing] }
  : name                                                   { [Src.Source emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)] }
  | name compositeTypingArgs                               { (Src.Source emptyInfos (tokenToArea $1) (Src.TRSingle $ strV $1)) : $2 }
  | name '.' name                                          { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRSingle $ (strV $1<>"."<>strV $3))] }
  | name '.' name compositeTypingArgs                      { (Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TRSingle $ (strV $1<>"."<>strV $3))) : $4 }
  | '(' typings ')' compositeTypingArgs                    { $2:$4 }
  | typing                                                 { [$1] }
  | '(' typing '->' typings ')'                     %shift { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TRArr $2 $4)] }
  | compositeTypingArgs '(' typing '->' typings ')' %shift { $1 <> [Src.Source emptyInfos (mergeAreas (Src.getArea $ head $1) (tokenToArea $6)) (Src.TRArr $3 $5)] }

recordTypingArgs :: { M.Map Src.Name Src.Typing }
  : name '::' typings                               { M.fromList [(strV $1, $3)] }
  | recordTypingArgs ',' name '::' typings          { M.insert (strV $3) $5 $1 }

tupleTypings :: { [Src.Typing] }
  : typing ',' typing                   { [$1, $3] }
  | typing ',' compositeTyping          { [$1, $3] }
  | compositeTyping ',' typing          { [$1, $3] }
  | compositeTyping ',' compositeTyping { [$1, $3] }
  | tupleTypings ',' typing             { $1 <> [$3] }
  | tupleTypings ',' compositeTyping    { $1 <> [$3] }

exp :: { Src.Exp }
  : literal                                                    { $1 }
  | jsx                                                        { $1 }
  | record                                              %shift { $1 }
  | where                                               %shift { $1 }
  | tupleConstructor                                    %shift { $1 }
  | operation                                                  { $1 }
  | templateString                                      %shift { $1 }
  | listConstructor                                     %shift { $1 }
  | typedExp                                            %shift { $1 }
  | js                                                  %shift { Src.Source emptyInfos (tokenToArea $1) (Src.JSExp (strV $1)) }
  | name '=' maybeRet exp                                        %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $4)) (Src.Assignment (strV $1) $4) }
  | name                                                %shift { Src.Source emptyInfos (tokenToArea $1) (Src.Var $ strV $1) }
  | '.' name                                            %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.Var $ '.':strV $2) }
  | 'pipe' '(' maybeRet args ')' '(' args ')'           %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $8)) (buildPipe (mergeAreas (tokenToArea $1) (tokenToArea $5)) $4) $7 }
  | 'pipe' '(' maybeRet args ')'                        %shift { buildPipe (mergeAreas (tokenToArea $1) (tokenToArea $5)) $4 }
  | app                                                 %shift { $1 }
  | '(' params ')' '=>' rets exp                        %shift { buildAbs (mergeAreas (tokenToArea $1) (Src.getArea $6)) $2 [$6] }
  | '(' params ')' '=>' '(' rets exp rets ')'           %shift { buildAbs (mergeAreas (tokenToArea $1) (tokenToArea $9)) $2 [$7] }
  | '(' params ')' '=>' '{' rets multiExpBody rets '}'  %shift { buildAbs (mergeAreas (tokenToArea $1) (tokenToArea $9)) $2 $7 }
  | '(' exp ')'                                         %shift { $2 }
  | exp '.' name                                        %shift { access $1 (Src.Source emptyInfos (tokenToArea $3) (Src.Var $ "." <> strV $3)) }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}'
      { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $17)) (Src.If $3 $7 $15) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet exp
      { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $10)) (Src.If $3 $6 $10) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet exp 'ret'
      { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $10)) (Src.If $3 $6 $10) }
  | exp '?' maybeRet exp maybeRet ':' maybeRet exp
      { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $8)) (Src.If $1 $4 $8) }
  | exp '?' maybeRet exp maybeRet ':' maybeRet exp 'ret'
      { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $8)) (Src.If $1 $4 $8) }


jsx :: { Src.Exp }
  : jsxTag { $1 }

jsxTags :: { [Src.Exp] }
  : jsxTag              { [$1] }
  | jsxTags rets jsxTag { $1 <> [$3] }

jsxTag :: { Src.Exp }
  : 'jsx<1' name jsxProps '/' 'tuple>' %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.JsxTag (strV $2) $3 []) }
  | 'jsx<1' name jsxProps '/' '>'      %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.JsxTag (strV $2) $3 []) }
  | 'jsx<' name jsxProps 'tuple>' rets jsxChildren rets 'jsx</' '/' name 'tuple>' %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $11)) (Src.JsxTag (strV $2) $3 $6) }
  | 'jsx<' name jsxProps 'tuple>' rets jsxChildren rets 'jsx</' '/' name '>'      %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $11)) (Src.JsxTag (strV $2) $3 $6) }
  | 'jsx<' name jsxProps '>' rets jsxChildren rets 'jsx</' '/' name '>'           %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $11)) (Src.JsxTag (strV $2) $3 $6) }
  | 'jsx<' name jsxProps '>' rets jsxChildren rets 'jsx</' '/' name 'tuple>'      %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $11)) (Src.JsxTag (strV $2) $3 $6) }

jsxProp :: { Src.JsxProp }
  : name '=' str          { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.JsxProp (strV $1) (Src.Source emptyInfos (tokenToArea $3) (Src.LStr $ strV $3))) }
  | name '=' '{' exp '}'  { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.JsxProp (strV $1) $4) }

jsxProps :: { [Src.JsxProp] }
  : jsxProp rets               { [$1] }
  | jsxProps rets jsxProp rets { $1 <> [$3] }
  | {- empty -}                { [] }

jsxChildren :: { [Src.Exp] }
  : jsxTag                        { [$1] }
  | jsxChildren rets jsxTag       { $1 <> [$3] }
  | '{' exp '}'                   %shift { [$2] }
  | jsxChildren rets '{' exp '}'  %shift { $1 <> [$4] }
  | rets names                    %shift { [Src.Source emptyInfos emptyArea (Src.LStr $ "\"" <> unwords $2 <> "\"")] }
  | jsxChildren rets names        %shift { $1 <> [Src.Source emptyInfos emptyArea (Src.LStr $ "\"" <> unwords $3 <> "\"")] }
  | rets                          %shift { [] }
  | {- empty -}                   %shift { [] }


templateString :: { Src.Exp }
  : strTplStart templateStringParts strTplEnd { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.TemplateString ($2 <> [Src.Source emptyInfos (tokenToArea $3) (Src.LStr (strV $3))])) }
  | strTplStart strTplEnd                     { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.TemplateString [Src.Source emptyInfos (tokenToArea $2) (Src.LStr (strV $2))]) }

templateStringParts :: { [Src.Exp] }
  : exp                      { [$1] }
  | templateStringParts exp  { $1 <> [$2] }

app :: { Src.Exp }
  : app '(' argsWithPlaceholder ')'          %shift { buildApp (mergeAreas (Src.getArea $1) (tokenToArea $4)) $1 $3 }
  | name '(' argsWithPlaceholder ')'         %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $4)) (Src.Source emptyInfos (tokenToArea $1) (Src.Var $ strV $1)) $3 }
  | '.' name '(' argsWithPlaceholder ')'     %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.Source emptyInfos (tokenToArea $2) (Src.Var $ '.':strV $2)) $4 }
  | name '(' 'ret' argsWithPlaceholder ')'   %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.Source emptyInfos (tokenToArea $1) (Src.Var $ strV $1)) $4 }
  | exp '(' argsWithPlaceholder ')'          %shift { buildApp (mergeAreas (Src.getArea $1) (tokenToArea $4)) $1 $3 }
  | '(' exp ')' '(' argsWithPlaceholder ')'  %shift { buildApp (mergeAreas (tokenToArea $1) (tokenToArea $6)) $2 $5 }
  | exp '.' name '(' argsWithPlaceholder ')' %shift { buildApp (mergeAreas (Src.getArea $1) (tokenToArea $6)) (access $1 (Src.Source emptyInfos (tokenToArea $3) (Src.Var $ "." <> strV $3))) $5 }

multiExpBody :: { [Src.Exp] }
  : 'return' exp          { [$2] }
  | exp rets multiExpBody { $1:$3 }

typedExp :: { Src.Exp }
  : '(' exp '::' typings ')'                       %shift { Src.Source emptyInfos (mergeAreas (Src.getArea $2) (Src.getArea $4)) (Src.TypedExp $2 $4) }
  | '(' name '::' typings ')'                      %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $2) (Src.getArea $4)) (Src.TypedExp (Src.Source emptyInfos (tokenToArea $2) (Src.Var (strV $2))) $4) }
  | name '::' constrainedTyping 'ret' name '=' exp %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $7)) (Src.TypedExp (Src.Source emptyInfos (mergeAreas (tokenToArea $5) (Src.getArea $7)) (Src.Assignment (strV $5) $7)) $3) }

where :: { Src.Exp }
  : 'where' '(' exp ')' '{' maybeRet iss maybeRet '}' %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $9)) (Src.Where $3 $7) }
  | 'where' '(' exp ')' maybeRet iss                  %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $ last $6)) (Src.Where $3 $6) }
  | 'where' '(' exp ')' maybeRet iss 'ret'            %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $ last $6)) (Src.Where $3 $6) }
  | 'where' '{' rets iss rets '}'                     %shift { buildAbs (mergeAreas (tokenToArea $1) (tokenToArea $6)) [Src.Source emptyInfos emptyArea "__x__"] [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $6)) (Src.Where (Src.Source emptyInfos emptyArea (Src.Var "__x__")) $4)] }
  | 'where' rets iss rets                             %shift { buildAbs (mergeAreas (tokenToArea $1) (Src.getArea $ last $3)) [Src.Source emptyInfos emptyArea "__x__"] [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $ last $3)) (Src.Where (Src.Source emptyInfos emptyArea (Src.Var "__x__")) $3)] }

iss :: { [Src.Is] }
  : 'is' pattern ':' maybeRet exp                    %shift { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $5)) (Src.Is $2 $5)] }
  | 'is' pattern ':' maybeRet exp 'ret'              %shift { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $5)) (Src.Is $2 $5)] }
  | iss maybeRet 'is' pattern ':' maybeRet exp       %shift { $1 <> [Src.Source emptyInfos (mergeAreas (tokenToArea $3) (Src.getArea $7)) (Src.Is $4 $7)] }
  | iss maybeRet 'is' pattern ':' maybeRet exp 'ret' %shift { $1 <> [Src.Source emptyInfos (mergeAreas (tokenToArea $3) (Src.getArea $7)) (Src.Is $4 $7)] }

pattern :: { Src.Pattern }
  : nonCompositePattern %shift { $1 }
  | compositePattern    %shift { $1 }

nonCompositePattern :: { Src.Pattern }
  : name             { nameToPattern (tokenToArea $1) (strV $1) }
  | number           { Src.Source emptyInfos (tokenToArea $1) (Src.PNum $ strV $1) }
  | str              { Src.Source emptyInfos (tokenToArea $1) (Src.PStr $ strV $1) }
  | true             { Src.Source emptyInfos (tokenToArea $1) (Src.PBool $ strV $1) }
  | false            { Src.Source emptyInfos (tokenToArea $1) (Src.PBool $ strV $1) }
  | recordPattern    { $1 }
  | listPattern      { $1 }
  | tuplePattern     { $1 }
  | '(' pattern ')'  { $2 }


compositePattern :: { Src.Pattern }
  : name patterns          %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea (last $2))) (Src.PCtor (strV $1) $2) }
  | name '.' name patterns %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea (last $4))) (Src.PCtor (strV $1 <> "." <> strV $3) $4) }
  | name '.' name          %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PCtor (strV $1 <> "." <> strV $3) []) }

patterns :: { [Src.Pattern] }
  : nonCompositePattern          { [$1] }
  | patterns nonCompositePattern { $1 <> [$2] }

recordPattern :: { Src.Pattern }
  : '{' recordFieldPatterns '}' %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PRecord $2) }

recordFieldPatterns :: { M.Map Src.Name Src.Pattern }
  : name ':' pattern                         { M.fromList [(strV $1, $3)] }
  | name                                     { M.fromList [(strV $1, Src.Source emptyInfos (tokenToArea $1) (Src.PVar (strV $1)))] }
  | recordFieldPatterns ',' name ':' pattern { M.insert (strV $3) $5 $1 }
  | recordFieldPatterns ',' name             { M.insert (strV $3) (Src.Source emptyInfos (tokenToArea $3) (Src.PVar (strV $3))) $1 }

listPattern :: { Src.Pattern }
  : '[' listItemPatterns ']' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PList $2) }

listItemPatterns :: { [Src.Pattern] }
  : pattern                            { [$1] }
  | listItemPatterns ',' pattern       { $1 <> [$3] }
  | listItemPatterns ',' spreadPattern { $1 <> [$3] }
  | {- empty -}                        { [] }

spreadPattern :: { Src.Pattern }
  : '...' name  { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) (Src.PSpread (nameToPattern (tokenToArea $2) (strV $2))) }


tuplePattern :: { Src.Pattern }
  : '<' tupleItemPatterns 'tuple>' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $3)) (Src.PTuple $2) }

tupleItemPatterns :: { [Src.Pattern] }
  : pattern                       %shift { [$1] }
  | tupleItemPatterns ',' pattern %shift { $1 <> [$3] }


record :: { Src.Exp }
  : '{' rets recordFields maybeComa rets '}' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $6)) (Src.Record $3) }

recordFields :: { [Src.Field] }
  : name ':' exp                            { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $3)) $ Src.Field (strV $1, $3)] }
  | '...' exp                               { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $2)) $ Src.FieldSpread $2] }
  | recordFields ',' name ':' exp           { $1 <> [Src.Source emptyInfos (mergeAreas (tokenToArea $3) (Src.getArea $5)) $ Src.Field (strV $3, $5)] }
  | recordFields rets ',' rets name ':' exp { $1 <> [Src.Source emptyInfos (mergeAreas (tokenToArea $5) (Src.getArea $7)) $ Src.Field (strV $5, $7)] }
  | recordFields ',' '...' exp              { $1 <> [Src.Source emptyInfos (mergeAreas (tokenToArea $3) (Src.getArea $4)) $ Src.FieldSpread $4] }
  | {- empty -}                             { [] }


operation :: { Src.Exp }
  : exp '+' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "+")) 
                         $1 False)))
                      $3 True)
                 }
  | exp '++' exp { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "++")) 
                         $1 False))) 
                      $3 True)
                 }
  | exp '-' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "-")) 
                         $1 False))) 
                      $3 True)
                 }
  | exp '*' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "*")) 
                         $1 False))) 
                      $3 True)
                 }
  | exp '/' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "/")) 
                         $1 False))) 
                      $3 True)
                 }
  | exp '%' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "%"))
                         $1 False)))
                      $3 True)
                 }
  | exp '==' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "==")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '!=' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "!=")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '&&' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "&&")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '||' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "||")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '>' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var ">")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '<' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "<")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '>=' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var ">=")) 
                         $1 False))) 
                      $3 True)
                   }
  | exp '<=' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "<="))
                         $1 False))) 
                      $3 True)
                   }
  | '!' exp  { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $2)) (Src.App (Src.Source emptyInfos (tokenToArea $1) (Src.Var "!")) $2 True) }
  | exp '|>' exp  { Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                      ((Src.Source emptyInfos (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.App
                         (Src.Source emptyInfos (tokenToArea $2) (Src.Var "|>")) 
                         $1 False))) 
                      $3 True)
                  
                  }

listConstructor :: { Src.Exp }
  : '[' rets listItems maybeComa rets ']' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $6)) (Src.ListConstructor $3) }

listItems :: { [Src.ListItem] }
  : exp                         { [Src.Source emptyInfos (Src.getArea $1) (Src.ListItem $1)] }
  | listItems ',' exp           { $1 <> [Src.Source emptyInfos (Src.getArea $3) (Src.ListItem $3)] }
  | listItems rets ',' rets exp { $1 <> [Src.Source emptyInfos (Src.getArea $5) (Src.ListItem $5)] }
  | '...' exp                   { [Src.Source emptyInfos (mergeAreas (tokenToArea $1) (Src.getArea $2)) (Src.ListSpread $2)] }
  | listItems ',' '...' exp     { $1 <> [Src.Source emptyInfos (mergeAreas (tokenToArea $3) (Src.getArea $4)) (Src.ListSpread $4)] }
  | {- empty -}                 { [] }

tupleConstructor :: { Src.Exp }
  : '<' maybeRet tupleItems maybeRet 'tuple>' { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $5)) (Src.TupleConstructor $3) }

tupleItems :: { [Src.Exp] }
  : exp maybeRet ',' maybeRet exp        %shift { [$1, $5] }
  | tupleItems maybeRet ',' maybeRet exp %shift { $1 <> [$5] }


literal :: { Src.Exp }
  : number  %shift { Src.Source emptyInfos (tokenToArea $1) (Src.LNum $ strV $1) }
  | str     %shift { Src.Source emptyInfos (tokenToArea $1) (Src.LStr $ strV $1) }
  | true    %shift { Src.Source emptyInfos (tokenToArea $1) (Src.LBool $ strV $1) }
  | false   %shift { Src.Source emptyInfos (tokenToArea $1) (Src.LBool $ strV $1) }
  | '(' ')' %shift { Src.Source emptyInfos (mergeAreas (tokenToArea $1) (tokenToArea $2)) Src.LUnit }

argsWithPlaceholder :: { [Src.Exp] }
  : '$'                                     %shift { [Src.Source emptyInfos (tokenToArea $1) (Src.Var "$")] }
  | '$' 'ret'                               %shift { [Src.Source emptyInfos (tokenToArea $1) (Src.Var "$")] }
  | '$' ',' argsWithPlaceholder             %shift { (Src.Source emptyInfos (tokenToArea $1) (Src.Var "$")):$3 }
  | '$' 'ret' ',' argsWithPlaceholder       %shift { (Src.Source emptyInfos (tokenToArea $1) (Src.Var "$")):$4 }
  | '$' ',' 'ret' argsWithPlaceholder       %shift { (Src.Source emptyInfos (tokenToArea $1) (Src.Var "$")):$4 }
  | '$' 'ret' ',' 'ret' argsWithPlaceholder %shift { (Src.Source emptyInfos (tokenToArea $1) (Src.Var "$")):$5 }

  | exp ',' argsWithPlaceholder             %shift { $1:$3 }
  | exp 'ret' ',' argsWithPlaceholder       %shift { $1:$4 }
  | exp ',' 'ret' argsWithPlaceholder       %shift { $1:$4 }
  | exp 'ret' ',' 'ret' argsWithPlaceholder %shift { $1:$5 }
  | exp                                     %shift { [$1] }
  | exp 'ret'                               %shift { [$1] }

args :: { [Src.Exp] }
  : exp ',' args             %shift { $1:$3 }
  | exp 'ret' ',' args       %shift { $1:$4 }
  | exp ',' 'ret' args       %shift { $1:$4 }
  | exp 'ret' ',' 'ret' args %shift { $1:$5 }
  | exp                      %shift { [$1] }
  | exp 'ret'                %shift { [$1] }

params :: { [Src.Source Src.Name] }
  : name ',' params %shift { (Src.Source emptyInfos (tokenToArea $1) (strV $1)) : $3 }
  | name            %shift { [Src.Source emptyInfos (tokenToArea $1) (strV $1)] }

{
buildAbs :: Area -> [Src.Source Src.Name] -> [Src.Exp] -> Src.Exp
buildAbs area params body = buildAbs' (length params) area params body


buildAbs' :: Int -> Area -> [Src.Source Src.Name] -> [Src.Exp] -> Src.Exp
buildAbs' nth area [param] body = Src.Source emptyInfos area (Src.Abs param body)
buildAbs' nth area (x:xs) body  = Src.Source emptyInfos area (Src.Abs x [(buildAbs' (nth + 1) area xs body)])

buildPipe :: Area -> [Src.Exp] -> Src.Exp
buildPipe area exps = Src.Source emptyInfos area (Src.Pipe exps)

buildApp :: Area -> Src.Exp -> [Src.Exp] -> Src.Exp
buildApp area f args = buildApp' (length args) (length args) area f args

buildApp' :: Int -> Int -> Area -> Src.Exp -> [Src.Exp] -> Src.Exp
buildApp' total nth area f@(Src.Source _ _ f') [(Src.Source emptyInfos area' arg)]  = Src.Source emptyInfos area (Src.App f (Src.Source Infos { origin = Just f', nthArg = Just nth } area' arg) (total == nth))
buildApp' total nth area f@(Src.Source _ _ f') xs     = 
  let (Src.Source emptyInfos area' arg) = last xs
      argWithMeta        = Src.Source Infos { origin = Just f', nthArg = Just nth } area' arg
  in  Src.Source emptyInfos (mergeAreas area area') (Src.App (buildApp' total (nth - 1) area f (init xs)) argWithMeta (total == nth))



mergeAreas :: Area -> Area -> Area
mergeAreas (Area l _) (Area _ r) = Area l r


nameToPattern :: Area -> String -> Src.Pattern
nameToPattern area n | n == "_"      = Src.Source emptyInfos area Src.PAny
                | n == "String"      = Src.Source emptyInfos area (Src.PCon n)
                | n == "Boolean"     = Src.Source emptyInfos area (Src.PCon n)
                | n == "Number"      = Src.Source emptyInfos area (Src.PCon n)
                | (isUpper . head) n = Src.Source emptyInfos area (Src.PCtor n [])
                | otherwise          = Src.Source emptyInfos area (Src.PVar n)

access :: Src.Exp -> Src.Exp -> Src.Exp
access src field = 
  Src.Source emptyInfos (mergeAreas (Src.getArea src) (Src.getArea field)) (Src.Access src field)


sanitizeImportPath :: String -> String
sanitizeImportPath = init . tail


lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap f = alexMonadScan >>= f

parseError :: Token -> Alex a
parseError (Token (Area (Loc a l c) _) cls) =
  alexError (printf "%d\n%d\nSyntax error - line: %d, column: %d\nThe following token is not valid: %s" l c l c (show cls))

parse :: String -> Either String Src.AST
parse s = runAlex s parseMadlib
}
