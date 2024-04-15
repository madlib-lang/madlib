{
module Parse.Madlib.Grammar where

import           Text.Printf
import           Control.Monad.Except
import qualified Data.Map                 as M
import qualified Data.List                as List
import           Data.Char(isUpper)

import           Parse.Madlib.Lexer
import           Infer.Type
import qualified AST.Source               as Src
import           Explain.Location
import           Run.Target
import           Debug.Trace
import           Text.Show.Pretty
}

%name parseMadlib ast
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexerWrap } { Token _ _ TokenEOF }

%token
  byte        { Token _ _ (TokenByte _) }
  short       { Token _ _ (TokenShort _) }
  int         { Token _ _ (TokenInt _) }
  number      { Token _ _ (TokenNumber _) }
  float       { Token _ _ (TokenFloat _) }
  str         { Token _ _ (TokenStr _) }
  char        { Token _ _ (TokenChar _) }
  strTplStart { Token _ _ (TokenTemplateStringStart) }
  strTplEnd   { Token _ _ (TokenTemplateStringEnd _) }
  name        { Token _ _ (TokenName _) }
  nameC       { Token _ _ (TokenConstraintName _) }
  js          { Token _ _ (TokenJSBlock _) }
  'ret'       { Token _ _ TokenReturn }
  '#'         { Token _ _ TokenSharpSign }
  '='         { Token _ _ TokenEq }
  ':='        { Token _ _ TokenMutateEq }
  '+'         { Token _ _ TokenPlus }
  '++'        { Token _ _ TokenDoublePlus }
  '-'         { Token _ _ TokenDash }
  '-unary'    { Token _ _ TokenDashUnary }
  '*'         { Token _ _ TokenStar }
  '/'         { Token _ _ TokenSlash }
  '%'         { Token _ _ TokenPercent }
  '::'        { Token _ _ TokenDoubleColon }
  ':'         { Token _ _ TokenColon }
  '?'         { Token _ _ TokenQuestionMark }
  '<-'        { Token _ _ TokenLeftArrow }
  '->'        { Token _ _ TokenRightArrow }
  '=>'        { Token _ _ TokenFatArrow }
  '.'         { Token _ _ TokenDot }
  ','         { Token _ _ TokenComma }
  '('         { Token _ _ TokenLeftParen }
  ')'         { Token _ _ TokenRightParen }
  '{{'        { Token _ _ TokenLeftDoubleCurly }
  '{'         { Token _ _ TokenLeftCurly }
  '}'         { Token _ _ TokenRightCurly }
  '#['        { Token _ _ TokenTupleStart }
  '['         { Token _ _ TokenLeftSquaredBracket }
  ']'         { Token _ _ TokenRightSquaredBracket }
  '$'         { Token _ _ TokenDollar }
  'if'        { Token _ _ TokenIf }
  'while'     { Token _ _ TokenWhile }
  'else'      { Token _ _ TokenElse }
  'interface' { Token _ _ TokenInterface }
  'instance'  { Token _ _ TokenInstance }
  'do'        { Token _ _ TokenDo }
  'where'     { Token _ _ TokenWhere }
  'return'    { Token _ _ TokenReturnKeyword }
  '=='        { Token _ _ TokenDoubleEq }
  '!='        { Token _ _ TokenExclamationMarkEq }
  false       { Token _ _ (TokenBool _) }
  true        { Token _ _ (TokenBool _) }
  'extern'    { Token _ _ TokenExtern }
  'derive'    { Token _ _ TokenDerive }
  'import'    { Token _ _ TokenImport }
  'export'    { Token _ _ TokenExport }
  'texport'   { Token _ _ TokenTypeExport }
  'from'      { Token _ _ TokenFrom }
  '|'         { Token _ _ TokenPipe }
  '&'         { Token _ _ TokenAmpersand }
  '^'         { Token _ _ TokenXor }
  '~'         { Token _ _ TokenTilde }
  '<<'        { Token _ _ TokenDoubleLeftChevron }
  '>>'        { Token _ _ TokenDoubleRightChevron }
  '>>>'       { Token _ _ TokenTripleRightChevron }
  'pipe'      { Token _ _ TokenPipeKeyword }
  '|>'        { Token _ _ TokenPipeOperator }
  '<|>'       { Token _ _ TokenAlternativeOperator }
  '...'       { Token _ _ TokenSpreadOperator }
  'type'      { Token _ _ TokenType }
  'alias'     { Token _ _ TokenAlias }
  '&&'        { Token _ _ TokenDoubleAmpersand }
  '||'        { Token _ _ TokenDoublePipe }
  '>'         { Token _ _ TokenRightChevron }
  '<'         { Token _ _ TokenLeftChevron }
  'jsx<'      { Token _ _ TokenJsxTagOpenStart }
  'jsx</'     { Token _ _ TokenJsxTagOpenEnd }
  'jsx<1'     { Token _ _ TokenJsxTagOpenSingle }
  '???'       { Token _ _ TokenTypedHole }

  '>='        { Token _ _ TokenRightChevronEq }
  '<='        { Token _ _ TokenLeftChevronEq }
  '!'         { Token _ _ TokenExclamationMark }

  '#iftarget' { Token _ _ (TokenMacroIfTarget) }
  '#elseif'   { Token _ _ (TokenMacroElseIf) }
  '#endif'    { Token _ _ TokenMacroEndIf }

%nonassoc LOWEST
-- %nonassoc ':='
-- %left ':' '->' '|' where is 'else' '='
%left ':' '->' '|' where 'else' '=' ':='
%left '?' 'if'
%left '<|>'
%left '|>'
%left '||'
%left '&&'
%left '>' '<' '>=' '<=' '==' '!='
%left '+' '-'
%left '*' '/' '%'
%left ','
%nonassoc '(' ')' '#[' '=>' '::' where 'ret' '{' '}' '[' ']'
%right '!'
%nonassoc HIGHEST
%left NEG
%%

ast :: { Src.AST }
  : typedecl ast              %shift { $2 { Src.atypedecls =  $1 : Src.atypedecls $2 } }
  | exp ast                   %shift { $2 { Src.aexps = $1 : Src.aexps $2 } }
  | bodyExp ast               %shift { $2 { Src.aexps = $1 : Src.aexps $2 } }
  | importDecls ast           %shift { $2 { Src.aimports = Src.aimports $2 <> $1, Src.apath = Nothing } }
  | interface ast             %shift { $2 { Src.ainterfaces = $1 : (Src.ainterfaces $2), Src.apath = Nothing } }
  | instance ast              %shift { $2 { Src.ainstances = $1 : (Src.ainstances $2), Src.apath = Nothing } }
  | {- empty -}               %shift { Src.AST { Src.aimports = [], Src.aderived = [], Src.aexps = [], Src.atypedecls = [], Src.ainterfaces = [], Src.ainstances = [], Src.apath = Nothing } }
  | 'ret'                     %shift { Src.AST { Src.aimports = [], Src.aderived = [], Src.aexps = [], Src.atypedecls = [], Src.ainterfaces = [], Src.ainstances = [], Src.apath = Nothing } }
  | 'ret' ast                 %shift { $2 }
  | '#iftarget' ast           %shift { $2 { Src.aexps = Src.Source (tokenArea $1) (tokenTarget $1) (Src.IfTarget (tokenTarget $1)) : Src.aexps $2 } }
  | '#elseif' ast             %shift { $2 { Src.aexps = Src.Source (tokenArea $1) (tokenTarget $1) (Src.ElseIfTarget (tokenTarget $1)) : Src.aexps $2 } }
  | '#endif' ast              %shift { $2 { Src.aexps = Src.Source (tokenArea $1) (tokenTarget $1) (Src.EndIfTarget) : Src.aexps $2 } }
  | 'export' name ast         %shift { $3 { Src.aexps = Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.NameExport $ strV $2) : Src.aexps $3 } }
  | 'texport' 'type' name ast %shift { $4 { Src.aexps = Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.TypeExport $ strV $3) : Src.aexps $4 } }
  | 'export' name '=' exp ast %shift { $5 { Src.aexps = (Src.Source (mergeAreas (tokenArea $1) (Src.getArea $4)) (tokenTarget $1) (Src.Export (Src.Source (mergeAreas (tokenArea $1) (Src.getArea $4)) (tokenTarget $1) (Src.Assignment (strV $2) $4)))) : Src.aexps $5 } }
  | name '::' constrainedTyping 'ret' 'export' name '=' exp ast
      %shift { $9 { Src.aexps = Src.Source (mergeAreas (tokenArea $1) (Src.getArea $8)) (tokenTarget $1) (Src.NamedTypedExp (strV $1) (Src.Source (mergeAreas (tokenArea $5) (Src.getArea $8)) (tokenTarget $1) (Src.Export (Src.Source (mergeAreas (tokenArea $6) (Src.getArea $8)) (tokenTarget $1) (Src.Assignment (strV $6) $8)))) $3) : Src.aexps $9 } }
  | 'derive' name name ast                          %shift { $4 { Src.aderived = Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.DerivedADT (strV $2) (strV $3)) : (Src.aderived $4) } }
  | 'derive' name '{' deriveDeclFieldNames '}' ast  %shift { $6 { Src.aderived = Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.DerivedRecord (strV $2) (fst $4)) : (Src.aderived $6) } }
  -- | error ast                        { $2 }


deriveDeclFieldNames :: { ([String], Area) }
  : name ',' deriveDeclFieldNames %shift { (strV $1 : fst $3, snd $3) }
  | name ',' name                 %shift { ([strV $1, strV $3], emptyArea) }
  | name                          %shift { ([strV $1], tokenArea $1) }


importDecls :: { [Src.Import] }
  : importDecl importDecls %shift { $1:$2 }
  | importDecl             %shift { [$1] }
  
importDecl :: { Src.Import }
  : 'import' '{' rets importNames maybeComma '}' 'from' str rets        { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.NamedImport $4 (sanitizeImportPath $ strV $8) (sanitizeImportPath $ strV $8)) }
  | 'import' name 'from' str rets                                       { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.DefaultImport (Src.Source (tokenArea $2) (tokenTarget $1) (strV $2)) (sanitizeImportPath $ strV $4) (sanitizeImportPath $ strV $4)) }
  | 'import' 'type' '{' rets importNames maybeComma '}' 'from' str rets { Src.Source (mergeAreas (tokenArea $1) (tokenArea $7)) (tokenTarget $1) (Src.TypeImport $5 (sanitizeImportPath $ strV $9) (sanitizeImportPath $ strV $9)) }
  -- TODO: reconsider if we really want global imports as that's receipe for name collisions and unpredictable behavior
  -- | 'import' str rets                                   { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.ImportAll (sanitizeImportPath $ strV $2) (sanitizeImportPath $ strV $2)) }

importNames :: { [Src.Source Src.Name] }
  : importNames ',' rets name %shift { $1 <> [Src.Source (tokenArea $4) (tokenTarget $4) (strV $4)] }
  | name                      %shift { [Src.Source (tokenArea $1) (tokenTarget $1) (strV $1)] }
  | {- empty -}               %shift { [] }


interface :: { Src.Interface }
  : 'interface' name names '{' rets methodDefs rets '}'                          { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.Interface [] (strV $2) $3 $6) }
  | 'interface' constraint '=>' name names '{' rets methodDefs rets '}'          { Src.Source (mergeAreas (tokenArea $1) (tokenArea $10)) (tokenTarget $1) (Src.Interface [$2] (strV $4) $5 $8) }
  | 'interface' '(' constraints ')' '=>' name names '{' rets methodDefs rets '}' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $12)) (tokenTarget $1) (Src.Interface $3 (strV $6) $7 $10) }


names :: { [Src.Name] }
  : name       { [strV $1] }
  | name names { (strV $1) : $2 }

methodDefs :: { M.Map Src.Name Src.Typing }
  : name '::' constrainedTyping                 { M.fromList [(strV $1, $3)] }
  | methodDefs rets name '::' constrainedTyping { M.union (M.fromList [(strV $3, $5)]) $1 }

instance :: { Src.Instance }
  : 'instance' name manyTypings '{{' rets methodImpls rets '}'                          %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.Instance [] (strV $2) $3 $6) }
  | 'instance' instanceConstraint '=>' name manyTypings '{{' rets methodImpls rets '}'           { Src.Source (mergeAreas (tokenArea $1) (tokenArea $10)) (tokenTarget $1) (Src.Instance [$2] (strV $4) $5 $8) }
  | 'instance' '(' maybeRet instanceConstraints ')' '=>' name manyTypings '{{' rets methodImpls rets '}' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $13)) (tokenTarget $1) (Src.Instance $4 (strV $7) $8 $11) }

methodImpls :: { M.Map Src.Name Src.Exp }
  : name '=' exp { M.fromList [(strV $1, Src.Source (mergeAreas (tokenArea $1) (Src.getArea $3)) (tokenTarget $1) (Src.Assignment (strV $1) $3))] }
  | methodImpls rets name '=' exp { M.union (M.fromList [(strV $3, Src.Source (mergeAreas (tokenArea $3) (Src.getArea $5)) (tokenTarget $3) (Src.Assignment (strV $3) $5))]) $1 }

rets :: { [TokenClass] }
  : 'ret'       %shift{ [] }
  | rets 'ret'  %shift{ [] }
  | {- empty -} %shift { [] }

maybeRet :: { [TokenClass] }
  : 'ret'       { [] }
  | {- empty -} { [] }

maybeComma :: { [TokenClass] }
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

nonNullCommaCount :: { Int }
  : ','            %shift { 1 }
  | commaCount ',' %shift { $1 + 1 }

commaCount :: { Int }
  : ','            %shift{ 1 }
  | commaCount ',' %shift{ $1 + 1 }
  | {- empty -}    %shift { 0 }


typedecl :: { Src.TypeDecl }
  : 'type' name typeParams rets '=' adtConstructors          %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $ last $6)) (tokenTarget $1) Src.ADT { Src.adtname = strV $2, Src.adtparams = $3, Src.adtconstructors = $6, Src.adtexported = False } }
  | 'export' 'type' name typeParams rets '=' adtConstructors %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $ last $7)) (tokenTarget $1) Src.ADT { Src.adtname = strV $3, Src.adtparams = $4, Src.adtconstructors = $7, Src.adtexported = True } }
  | 'alias' name typeParams rets '=' typings                 %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $6)) (tokenTarget $1) Src.Alias { Src.aliasname = strV $2, Src.aliasparams = $3, Src.aliastype = $6, Src.aliasexported = False } }
  | 'export' 'alias' name typeParams rets '=' typings        %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $7)) (tokenTarget $1) Src.Alias { Src.aliasname = strV $3, Src.aliasparams = $4, Src.aliastype = $7, Src.aliasexported = True } }

typeParams :: { [Src.Name] }
  : name typeParams %shift { strV $1 : $2 }
  | {- empty -}               { [] }

adtConstructors :: { [Src.Constructor] }
  : adtConstructor rPipe adtConstructors      %shift { $1:$3 }
  | adtConstructor maybeRet                      %shift { [$1] }

adtConstructor :: { Src.Constructor }
  : name '(' rets adtConstructorArgs maybeComma rets ')' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $7)) (tokenTarget $1) (Src.Constructor (strV $1) $4) }
  | name                                                 %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.Constructor (strV $1) []) }

adtConstructorArgs :: { [Src.Typing] }
  : typing                                  %shift { [$1] }
  | compositeTyping                         %shift { [$1] }
  | typings                                 %shift { [$1] }
  | adtConstructorArgs ',' typing           %shift { $1 <> [$3] }
  | adtConstructorArgs ',' compositeTyping  %shift { $1 <> [$3] }
  | adtConstructorArgs ',' typings          %shift { $1 <> [$3] }


constrainedTyping :: { Src.Typing }
  : constraint '=>' typings          %shift { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.TRConstrained [$1] $3) }
  -- | '(' constraint ')' '=>' typings  %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $5)) (tokenTarget $1) (Src.TRConstrained [$2] $5) }
  | '(' constraints ')' '=>' typings %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $5)) (tokenTarget $1) (Src.TRConstrained $2 $5) }
  | typings                          %shift { $1 }

instanceConstraints :: { [Src.Typing] }
  : instanceConstraint                                  { [$1] }
  | instanceConstraints ',' maybeRet instanceConstraint { $1 <> [$4] }

instanceConstraint :: { Src.Typing }
  : nameC nameC                 { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2))]) }
  | nameC nameC nameC           { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2)), Src.Source (tokenArea $3) (tokenTarget $1) (Src.TRSingle (strV $3))]) }
  | nameC nameC nameC nameC     { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2)), Src.Source (tokenArea $3) (tokenTarget $1) (Src.TRSingle (strV $3)), Src.Source (tokenArea $4) (tokenTarget $1) (Src.TRSingle (strV $4))]) }
  | nameC nameC '(' typings ')' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2)), $4]) }
  | nameC '(' typings ')' nameC { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRComp (strV $1) [$3, Src.Source (tokenArea $5) (tokenTarget $1) (Src.TRSingle (strV $5))]) }

constraints :: { [Src.Typing] }
  : constraint { [$1] }
  | constraints ',' constraint { $1 <> [$3] }

constraint :: { Src.Typing }
  : name name                 { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2))]) }
  | name name name            { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2)), Src.Source (tokenArea $3) (tokenTarget $1) (Src.TRSingle (strV $3))]) }
  | name name name name       { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2)), Src.Source (tokenArea $3) (tokenTarget $1) (Src.TRSingle (strV $3)), Src.Source (tokenArea $4) (tokenTarget $1) (Src.TRSingle (strV $4))]) }
  | name name '(' typings ')' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRComp (strV $1) [Src.Source (tokenArea $2) (tokenTarget $1) (Src.TRSingle (strV $2)), $4]) }
  | name '(' typings ')' name { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRComp (strV $1) [$3, Src.Source (tokenArea $5) (tokenTarget $1) (Src.TRSingle (strV $5))]) }


simpleTypings :: { [Src.Typing] }
  : name               %shift { [Src.Source (tokenArea $1) (tokenTarget $1) (Src.TRSingle $ strV $1)] }
  | name simpleTypings %shift { [Src.Source (tokenArea $1) (tokenTarget $1) (Src.TRSingle $ strV $1)] <> $2 }

manyTypings :: { [Src.Typing] }
  : typing              { [$1] }
  | typing manyTypings  { $1:$2 }

typings :: { Src.Typing }
  : typings '->' typings         %shift { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (tokenTarget $2) (Src.TRArr $1 $3) }
  | compositeTyping '->' typings %shift { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (tokenTarget $2) (Src.TRArr $1 $3) }
  | compositeTyping              %shift { $1 }
  | typing                       %shift { $1 }

typing :: { Src.Typing }
  : name                                                     %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.TRSingle $ strV $1) }
  | '{' '}'                                                  %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.TRSingle "{}") }
  | '(' typings ')'                                          %shift { $2 }
  | '{' rets recordTypingArgs maybeComma '}'                 %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRRecord $3 Nothing) }
  | '{' rets '...' name ','  recordTypingArgs maybeComma '}' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.TRRecord $6 (Just (Src.Source (mergeAreas (tokenArea $3) (tokenArea $4)) (tokenTarget $1) (Src.TRSingle $ strV $4)))) }
  | '{' rets '...' name maybeComma '}'                       %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.TRRecord mempty (Just (Src.Source (mergeAreas (tokenArea $3) (tokenArea $4)) (tokenTarget $1) (Src.TRSingle $ strV $4)))) }
  | '#[' tupleTypings maybeComma ']'                         %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.TRTuple $2) }

compositeTyping :: { Src.Typing }
  : name compositeTypingArgs          { Src.Source (mergeAreas (tokenArea $1) (Src.getArea (last $2))) (tokenTarget $1) (Src.TRComp (strV $1) $2) }
  | name '.' name compositeTypingArgs { Src.Source (mergeAreas (tokenArea $1) (Src.getArea (last $4))) (tokenTarget $1) (Src.TRComp (strV $1<>"."<>strV $3) $4) }
  | name '.' name                     { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.TRComp (strV $1<>"."<>strV $3) []) }
  | '(' name rets compositeTypingArgs ')'          { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRComp (strV $2) $4) }
  | '(' name '.' name rets compositeTypingArgs ')' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $7)) (tokenTarget $1) (Src.TRComp (strV $2<>"."<>strV $4) $6) }

compositeTypingArgs :: { [Src.Typing] }
  : name                                                   { [Src.Source (tokenArea $1) (tokenTarget $1) (Src.TRSingle $ strV $1)] }
  | name compositeTypingArgs                               { (Src.Source (tokenArea $1) (tokenTarget $1) (Src.TRSingle $ strV $1)) : $2 }
  | name '.' name                                          { [Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.TRSingle $ (strV $1<>"."<>strV $3))] }
  | name '.' name compositeTypingArgs                      { (Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.TRSingle $ (strV $1<>"."<>strV $3))) : $4 }
  | typing                                          %shift { [$1] }
  | typing compositeTypingArgs                      %shift { $1:$2 }
  | '(' typing '->' typings ')'                     %shift { [Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TRArr $2 $4)] }
  | compositeTypingArgs '(' typing '->' typings ')' %shift { $1 <> [Src.Source (mergeAreas (Src.getArea $ head $1) (tokenArea $6)) (tokenTarget $2) (Src.TRArr $3 $5)] }

recordTypingArgs :: { M.Map Src.Name (Area, Src.Typing) }
  : name '::' typings                               { M.fromList [(strV $1, (mergeAreas (tokenArea $1) (tokenArea $2), $3))] }
  | name '::' typings ','                           { M.fromList [(strV $1, (mergeAreas (tokenArea $1) (tokenArea $2), $3))] }
  | recordTypingArgs ',' name '::' typings          { M.insert (strV $3) (mergeAreas (tokenArea $3) (tokenArea $4), $5) $1 }
  | recordTypingArgs ',' name '::' typings ','      { M.insert (strV $3) (mergeAreas (tokenArea $3) (tokenArea $4), $5) $1 }

tupleTypings :: { [Src.Typing] }
  : typings                             %shift { [$1] }
  | tupleTypings ',' typings            %shift { $1 <> [$3] }
  | typings ',' typings                 %shift { [$1, $3] }
  -- | typing ',' typing                   %shift { [$1, $3] }
  -- | typing ',' compositeTyping          %shift { [$1, $3] }
  -- | compositeTyping ',' typing          %shift { [$1, $3] }
  -- | compositeTyping ',' compositeTyping %shift { [$1, $3] }
  -- | tupleTypings ',' typing             %shift { $1 <> [$3] }
  -- | tupleTypings ',' compositeTyping    %shift { $1 <> [$3] }


bodyExp :: { Src.Exp }
  : name '=' maybeRet exp                          %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $4)) (tokenTarget $1) (Src.Assignment (strV $1) $4) }
  | name '::' constrainedTyping 'ret' name '=' exp %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $7)) (tokenTarget $1) (Src.NamedTypedExp (strV $1) (Src.Source (mergeAreas (tokenArea $5) (Src.getArea $7)) (tokenTarget $1) (Src.Assignment (strV $5) $7)) $3) }
  | exp ':=' maybeRet exp     { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $4)) (Src.getSourceTarget $1) (Src.Mutate $1 $4) }
  | exp                                                   { $1 }


exp :: { Src.Exp }
  : literal                                                         { $1 }
  | jsx                                                             { $1 }
  | record                                                   %shift { $1 }
  | dict                                                     %shift { $1 }
  | where                                                    %shift { $1 }
  | while                                                    %shift { $1 }
  | do                                                       %shift { $1 }
  | tupleConstructor                                         %shift { $1 }
  | operation                                                       { $1 }
  | templateString                                           %shift { $1 }
  | listConstructor                                          %shift { $1 }
  | extern                                                   %shift { $1 }
  | typedExp                                                 %shift { $1 }
  | exp '[' exp ']'                                          %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $4)) (Src.getSourceTarget $1) (Src.ArrayAccess $1 $3) }
  | '???'                                                    %shift { Src.Source (tokenArea $1) (tokenTarget $1) Src.TypedHole }
  | js                                                       %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.JSExp (strV $1)) }
  | '#' name                                                 %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $2) (Src.Var $ '#' : strV $2) }
  | name                                                     %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var $ strV $1) }
  | '.' name                                                        { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.Var $ '.':strV $2) }
  | 'pipe' '(' maybeRet args ')' '(' argsWithPlaceholder ')' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.App (buildPipe (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) $4) $7) }
  | 'pipe' '(' maybeRet args ')'                             %shift { buildPipe (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) $4 }
  | app                                                      %shift { $1 }
  | absOrParenthesizedName                                   %shift { $1 }
  | '(' exp ')'                                              %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.Parenthesized (tokenArea $1) $2 (tokenArea $3)) }
  | exp '.' name                                                    { access $1 (Src.Source (tokenArea $3) (Src.getSourceTarget $1) (Src.Var $ "." <> strV $3)) }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}'
      { Src.Source (mergeAreas (tokenArea $1) (tokenArea $17)) (tokenTarget $1) (Src.If $3 $7 $15) }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet
      %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.If $3 $7 (Src.Source (Src.getArea $7) (tokenTarget $1) Src.LUnit)) }
  | 'if' '(' exp ')' maybeRet exp maybeRet
      %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $6)) (tokenTarget $1) (Src.If $3 $6 (Src.Source (Src.getArea $6) (tokenTarget $1) Src.LUnit)) }
  | 'if' '(' exp ')' '{' maybeRet exp maybeRet '}' maybeRet 'else' maybeRet maybeRet exp maybeRet
      { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $14)) (tokenTarget $1) (Src.If $3 $7 $14) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet '{' maybeRet exp maybeRet '}'
      { Src.Source (mergeAreas (tokenArea $1) (tokenArea $14)) (tokenTarget $1) (Src.If $3 $6 $12) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet exp
      { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $10)) (tokenTarget $1) (Src.If $3 $6 $10) }
  | 'if' '(' exp ')' maybeRet exp maybeRet 'else' maybeRet exp 'ret'
      { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $10)) (tokenTarget $1) (Src.If $3 $6 $10) }
  | exp '?' maybeRet exp maybeRet ':' maybeRet exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $8)) (Src.getSourceTarget $1) (Src.Ternary $1 $4 $8) }
  | exp '?' maybeRet exp maybeRet ':' maybeRet exp 'ret'
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $8)) (Src.getSourceTarget $1) (Src.Ternary $1 $4 $8) }


absOrParenthesizedName :: { Src.Exp }
  : '(' param ')' maybeAbsExps                         %shift { buildAbsOrParenthesizedName (tokenArea $1) (tokenArea $3) (tokenTarget $1) $2 $4 }
  | '(' params ')' '=>' rets exp                       %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $6)) (tokenTarget $1) (Src.Abs $2 [$6]) }
  | '(' params ')' '=>' '{' rets multiExpBody rets '}' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.AbsWithMultilineBody $2 $7) }
  -- Nullary sugar
  | '(' ')' '=>' rets exp                              %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $5)) (tokenTarget $1) (Src.Abs [] [$5]) }
  | '(' ')' '=>' '{' rets multiExpBody rets '}'        %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.AbsWithMultilineBody [] $6) }

maybeAbsExps :: { Maybe (Bool, Area, [Src.Exp]) }
  : '=>' rets bodyExp                   %shift { Just (False, mergeAreas (tokenArea $1) (Src.getArea $3), [$3]) }
  | '=>' '{' rets multiExpBody rets '}' %shift { Just (True, mergeAreas (tokenArea $1) (tokenArea $6), $4) }
  | {- nothing -}                       %shift { Nothing }


do :: { Src.Exp }
  : 'do' '{' rets doExps rets '}' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.Do $4) }

doExps :: { [Src.Exp] }
  : 'return' exp              %shift { [Src.Source (mergeAreas (tokenArea $1) (Src.getArea $2)) (tokenTarget $1) (Src.Return $2)] }
  | {- empty -}               %shift { [Src.Source emptyArea Src.TargetAll Src.LUnit] }
  | name '<-' exp rets doExps %shift { [Src.Source (mergeAreas (tokenArea $1) (Src.getArea $3)) (tokenTarget $1) (Src.DoAssignment (strV $1) $3)] <> $5 }
  | bodyExp rets doExps       %shift { [$1] <> $3 }


jsx :: { Src.Exp }
  : jsxTag { $1 }

jsxTags :: { [Src.Exp] }
  : jsxTag              { [$1] }
  | jsxTags rets jsxTag { $1 <> [$3] }

jsxTag :: { Src.Exp }
  : 'jsx<1' name jsxProps '/' '>'                                            %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.JsxAutoClosedTag (strV $2) $3) }
  | 'jsx<' name jsxProps '>' rets jsxChildren rets 'jsx</' '/' name '>'      %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $11)) (tokenTarget $1) (Src.JsxTag (strV $2) $3 $6) }

jsxProp :: { Src.JsxProp }
  : name '=' str          { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.JsxProp (strV $1) (Src.Source (tokenArea $3) (tokenTarget $3) (Src.LStr $ strV $3))) }
  | name '=' '{' exp '}'  { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.JsxProp (strV $1) $4) }
  | name                  { Src.Source (tokenArea $1) (tokenTarget $1) (Src.JsxProp (strV $1) (Src.Source (tokenArea $1) (tokenTarget $1) (Src.LBool "true"))) }

jsxProps :: { [Src.JsxProp] }
  : jsxProp rets               { [$1] }
  | jsxProps rets jsxProp rets { $1 <> [$3] }
  | {- empty -}                { [] }

jsxChildren :: { [Src.JsxChild] }
  : jsxTag                                    { [Src.JsxChild $1] }
  | jsxChildren rets jsxTag                   { $1 <> [Src.JsxChild $3] }
  | '{' exp '}'                        %shift { [Src.JsxExpChild $2] }
  | jsxChildren rets '{' exp '}'       %shift { $1 <> [Src.JsxExpChild $4] }
  | '{' '...' exp '}'                  %shift { [Src.JsxSpreadChild $3] }
  | jsxChildren rets '{' '...' exp '}' %shift { $1 <> [Src.JsxSpreadChild $5] }
  | rets names                         %shift { [Src.JsxChild (Src.Source emptyArea Src.TargetAll (Src.LStr $ "\"" <> unwords $2 <> "\""))] }
  | jsxChildren rets names             %shift { $1 <> [Src.JsxChild (Src.Source emptyArea Src.TargetAll (Src.LStr $ "\"" <> unwords $3 <> "\""))] }
  | rets                               %shift { [] }
  | {- empty -}                        %shift { [] }


templateString :: { Src.Exp }
  : strTplStart rets templateStringParts rets strTplEnd { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.TemplateString ($3 <> [Src.Source (tokenArea $5) (tokenTarget $1) (Src.LStr (strV $5))])) }
  | strTplStart strTplEnd                               { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.TemplateString [Src.Source (tokenArea $2) (tokenTarget $1) (Src.LStr (strV $2))]) }

templateStringParts :: { [Src.Exp] }
  : exp                          { [$1] }
  | templateStringParts rets exp { $1 <> [$3] }

app :: { Src.Exp }
  : app '(' rets argsWithPlaceholder ')'          %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $5)) (Src.getSourceTarget $1) (Src.App $1 $4) }
  | name '(' rets argsWithPlaceholder ')'         %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.App (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var $ strV $1)) $4) }
  | '.' name '(' rets argsWithPlaceholder ')'     %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.App (Src.Source (tokenArea $2) (tokenTarget $1) (Src.Var $ '.':strV $2)) $5) }
  | name '(' 'ret' argsWithPlaceholder ')'        %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.App (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var $ strV $1)) $4) }
  | exp '(' argsWithPlaceholder ')'               %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $4)) (Src.getSourceTarget $1) (Src.App $1 $3) }
  | '(' exp ')' '(' rets argsWithPlaceholder ')'  %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $7)) (tokenTarget $1) (Src.App (Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.Parenthesized (tokenArea $1) $2 (tokenArea $3))) $6) }
  | exp '.' name '(' rets argsWithPlaceholder ')' %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $7)) (Src.getSourceTarget $1) (Src.App (access $1 (Src.Source (tokenArea $3) (Src.getSourceTarget $1) (Src.Var $ "." <> strV $3))) $6) }

  -- Nullary sugar
  | app '(' ')'                               %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $3)) (Src.getSourceTarget $1) (Src.App $1 []) }
  | name '(' ')'                              %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.App (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var $ strV $1)) []) }
  | '.' name '(' ')'                          %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.App (Src.Source (tokenArea $2) (tokenTarget $1) (Src.Var $ '.':strV $2)) []) }
  | exp '(' ')'                               %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $3)) (Src.getSourceTarget $1) (Src.App $1 []) }
  | '(' exp ')' '(' ')'                       %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $5)) (tokenTarget $1) (Src.App (Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.Parenthesized (tokenArea $1) $2 (tokenArea $3))) []) }
  | exp '.' name '(' ')'                      %shift { Src.Source (mergeAreas (Src.getArea $1) (tokenArea $5)) (Src.getSourceTarget $1) (Src.App (access $1 (Src.Source (tokenArea $3) (Src.getSourceTarget $1) (Src.Var $ "." <> strV $3))) []) }

multiExpBody :: { [Src.Exp] }
  : 'return' exp              { [Src.Source (mergeAreas (tokenArea $1) (Src.getArea $2)) (tokenTarget $1) (Src.Return $2)] }
  | {- empty -}               { [Src.Source emptyArea Src.TargetAll Src.LUnit] }
  -- | lhs ':=' maybeRet exp rets multiExpBody    %shift { (Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $4)) (Src.getSourceTarget $1) (Src.Mutate $1 $4)) : $6 }
  | bodyExp rets multiExpBody { $1 : $3 }

typedExp :: { Src.Exp }
  : '(' exp '::' typings ')'                       %shift { Src.Source (mergeAreas (Src.getArea $2) (Src.getArea $4)) (tokenTarget $1) (Src.TypedExp $2 $4) }
  | '(' name '::' typings ')'                      %shift { Src.Source (mergeAreas (tokenArea $2) (Src.getArea $4)) (tokenTarget $1) (Src.TypedExp (Src.Source (tokenArea $2) (tokenTarget $1) (Src.Var (strV $2))) $4) }

extern :: { Src.Exp }
  : name '::' constrainedTyping 'ret' name '=' 'extern' str          %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $8)) (tokenTarget $1) (Src.Extern $3 (strV $5) (sanitizeImportPath $ strV $8)) }
  | name '::' constrainedTyping 'ret' 'export' name '=' 'extern' str        { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.Export (Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.Extern $3 (strV $6) (sanitizeImportPath $ strV $9)))) }

while :: { Src.Exp }
  : 'while' '(' exp ')' '{' rets exp rets  '}'       %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.While $3 $7) }
  | 'while' '(' exp ')' maybeRet exp maybeRet       %shift { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $6)) (tokenTarget $1) (Src.While $3 $6) }

where :: { Src.Exp }
  : 'where' '(' exp ')' '{' maybeRet iss maybeRet '}' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.Where $3 $7) }
  | 'where' '{' rets iss rets '}'                     %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.WhereAbs $4) }

iss :: { [Src.Is] }
  : pattern '=>' maybeRet exp rets              %shift { [Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $4)) (Src.getSourceTarget $1) (Src.Is $1 $4)] }
  | pattern '=>' maybeRet exp                   %shift { [Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $4)) (Src.getSourceTarget $1) (Src.Is $1 $4)] }
  | iss maybeRet pattern '=>' maybeRet exp      %shift { $1 <> [Src.Source (mergeAreas (Src.getArea $3) (Src.getArea $6)) (Src.getSourceTarget $3) (Src.Is $3 $6)] }
  | iss maybeRet pattern '=>' maybeRet exp rets %shift { $1 <> [Src.Source (mergeAreas (Src.getArea $3) (Src.getArea $6)) (Src.getSourceTarget $3) (Src.Is $3 $6)] }

pattern :: { Src.Pattern }
  : nonCompositePattern %shift { $1 }
  | compositePattern    %shift { $1 }

nonCompositePattern :: { Src.Pattern }
  : name             { nameToPattern (tokenArea $1) (tokenTarget $1) (strV $1) }
  | number           { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PNum $ strV $1) }
  | float            { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PFloat $ strV $1)}
  | '-unary' number  { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PNum $ '-' : strV $2) }
  | '-unary' float   { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PFloat $ '-' : strV $2)}
  | str              { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PStr $ strV $1) }
  | char             { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PChar $ charData $1) }
  | true             { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PBool $ strV $1) }
  | false            { Src.Source (tokenArea $1) (tokenTarget $1) (Src.PBool $ strV $1) }
  | recordPattern    { $1 }
  | listPattern      { $1 }
  | tuplePattern     { $1 }
  | '(' pattern ')'  { $2 }


compositePattern :: { Src.Pattern }
  : name '(' rets compositePatternArgs maybeComma rets ')'          { Src.Source (mergeAreas (tokenArea $1) (tokenArea $7)) (tokenTarget $1) (Src.PCon (Src.Source (tokenArea $1) (tokenTarget $1) (strV $1)) $4) }
  | name '.' name '(' rets compositePatternArgs maybeComma rets ')' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.PCon (Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) $ strV $1 <> "." <> strV $3) $6) }
  | name '.' name                                                   { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.PNullaryCon (Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) $ strV $1 <> "." <> strV $3)) }

compositePatternArgs :: { [Src.Pattern] }
  : pattern maybeComma rets                               { [$1] }
  | pattern ',' rets compositePatternArgs                 { $1:$4 }
  | pattern ',' rets compositePatternArgs maybeComma rets { $1:$4 }

recordPattern :: { Src.Pattern }
  : '{' recordFieldPatterns maybeComma '}' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.PRecord $2) }

recordFieldPatterns :: { [Src.PatternField] }
  : name ':' pattern                         { [Src.PatternField (Src.Source (tokenArea $1) (tokenTarget $1) (strV $1)) $3] }
  | name                                     { [Src.PatternFieldShorthand (Src.Source (tokenArea $1) (tokenTarget $1) (strV $1))] }
  | recordFieldPatterns ',' name ':' pattern { $1 <> [Src.PatternField (Src.Source (tokenArea $3) (tokenTarget $3) (strV $3)) $5] }
  | recordFieldPatterns ',' name             { $1 <> [Src.PatternFieldShorthand (Src.Source (tokenArea $3) (tokenTarget $3) (strV $3))] }

listPattern :: { Src.Pattern }
  : '[' listItemPatterns maybeComma ']' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.PList $2) }

listItemPatterns :: { [Src.Pattern] }
  : pattern                            { [$1] }
  | listItemPatterns ',' pattern       { $1 <> [$3] }
  | listItemPatterns ',' spreadPattern { $1 <> [$3] }
  | {- empty -}                        { [] }

spreadPattern :: { Src.Pattern }
  : '...' name  { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) (Src.PSpread (nameToPattern (tokenArea $2) (tokenTarget $2) (strV $2))) }


tuplePattern :: { Src.Pattern }
  : '#[' tupleItemPatterns maybeComma ']' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $4)) (tokenTarget $1) (Src.PTuple $2) }

tupleItemPatterns :: { [Src.Pattern] }
  : pattern                       %shift { [$1] }
  | tupleItemPatterns ',' pattern %shift { $1 <> [$3] }


record :: { Src.Exp }
  : '{' rets recordFields maybeComma rets '}' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.Record $3) }
  | '{' rets '...' exp ',' recordFields maybeComma rets '}'
        { Src.Source (mergeAreas (tokenArea $1) (tokenArea $9)) (tokenTarget $1) (Src.Record ((Src.Source (mergeAreas (tokenArea $3) (Src.getArea $4)) (tokenTarget $1) $ Src.FieldSpread $4) : $6)) }

recordFields :: { [Src.Field] }
  : name ':' exp                            { [Src.Source (mergeAreas (tokenArea $1) (Src.getArea $3)) (tokenTarget $1) $ Src.Field (strV $1, $3)] }
  | name                                    { [Src.Source (tokenArea $1) (tokenTarget $1) $ Src.FieldShorthand (strV $1)] }
  | recordFields ',' name                   { $1 <> [Src.Source (tokenArea $3) (tokenTarget $3) $ Src.FieldShorthand (strV $3)] }
  | recordFields ',' name ':' exp           { $1 <> [Src.Source (mergeAreas (tokenArea $3) (Src.getArea $5)) (tokenTarget $3) $ Src.Field (strV $3, $5)] }
  | recordFields rets ',' rets name ':' exp { $1 <> [Src.Source (mergeAreas (tokenArea $5) (Src.getArea $7)) (tokenTarget $5) $ Src.Field (strV $5, $7)] }
  | {- empty -}                             { [] }

dict :: { Src.Exp }
  : '{{' rets dictItems maybeComma rets '}' '}' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $7)) (tokenTarget $1) (Src.Dictionary $3) }
  | '{{' '}' '}'                                { Src.Source (mergeAreas (tokenArea $1) (tokenArea $3)) (tokenTarget $1) (Src.Dictionary []) }

dictItems :: { [Src.DictItem] }
  : exp ':' exp                            { [Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) $ Src.DictItem $1 $3] }
  | dictItems ',' exp ':' exp              { $1 <> [Src.Source (mergeAreas (Src.getArea $3) (Src.getArea $5)) (Src.getSourceTarget $3) $ Src.DictItem $3 $5] }
  | dictItems rets ',' rets exp ':' exp    { $1 <> [Src.Source (mergeAreas (Src.getArea $5) (Src.getArea $7)) (Src.getSourceTarget $5) $ Src.DictItem $5 $7] }


operation :: { Src.Exp }
  : exp '+' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "+")) $3) }
  | exp '++' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "++")) $3) }
  | '-unary' exp %prec NEG
      { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $2)) (tokenTarget $1) (Src.UnOp (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "unary-minus")) $2) }
  | exp '-' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "-")) $3) }
  | exp '*' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "*")) $3) }
  | exp '/' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "/")) $3) }
  | exp '%' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "%")) $3) }
  | exp '==' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "==")) $3) }
  | exp '!=' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "!=")) $3) }
  | exp '&&' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "&&")) $3) }
  | exp '||' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "||")) $3) }
  | exp '>' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var ">")) $3) }
  | exp '<' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "<")) $3) }
  | exp '>=' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var ">=")) $3) }
  | exp '<=' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "<=")) $3) }
  | exp '|' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "|")) $3) }
  | exp '&' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "&")) $3) }
  | exp '^' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "^")) $3) }
  | exp '<<' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "<<")) $3) }
  | exp '>>' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var ">>")) $3) }
  | exp '>>>' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var ">>>")) $3) }
  | '!' exp
      { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $2)) (tokenTarget $1) (Src.UnOp (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "!")) $2) }
  | '~' exp
      { Src.Source (mergeAreas (tokenArea $1) (Src.getArea $2)) (tokenTarget $1) (Src.UnOp (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "~")) $2) }
  | exp '<|>' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "<|>")) $3) }
  | exp '|>' exp
      { Src.Source (mergeAreas (Src.getArea $1) (Src.getArea $3)) (Src.getSourceTarget $1) (Src.BinOp $1 (Src.Source (tokenArea $2) (tokenTarget $2) (Src.Var "|>")) $3) }

listConstructor :: { Src.Exp }
  : '[' rets listItems maybeComma rets ']' { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.ListConstructor $3) }

listItems :: { [Src.ListItem] }
  : exp                         { [Src.Source (Src.getArea $1) (Src.getSourceTarget $1) (Src.ListItem $1)] }
  | listItems ',' exp           { $1 <> [Src.Source (Src.getArea $3) (Src.getSourceTarget $3) (Src.ListItem $3)] }
  | listItems rets ',' rets exp { $1 <> [Src.Source (Src.getArea $5) (Src.getSourceTarget $5) (Src.ListItem $5)] }
  | '...' exp                   { [Src.Source (mergeAreas (tokenArea $1) (Src.getArea $2)) (tokenTarget $1) (Src.ListSpread $2)] }
  | listItems ',' '...' exp     { $1 <> [Src.Source (mergeAreas (tokenArea $3) (Src.getArea $4)) (tokenTarget $3) (Src.ListSpread $4)] }
  | {- empty -}                 { [] }

tupleConstructor :: { Src.Exp }
  : '#[' rets tupleItems maybeComma rets ']' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $6)) (tokenTarget $1) (Src.TupleConstructor $3) }

tupleItems :: { [Src.Exp] }
  : exp rets ',' rets exp        { [$1, $5] }
  | tupleItems rets ',' rets exp { $1 <> [$5] }
  | tupleItems ',' exp           { $1 <> [$3] }


literal :: { Src.Exp }
  : number  %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LNum $ strV $1) }
  | byte    %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LByte $ init $ init (strV $1)) }
  | short   %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LShort $ init $ init (strV $1)) }
  | int     %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LInt $ init $ init (strV $1)) }
  | float   %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LFloat $ strV $1) }
  | str     %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LStr $ strV $1) }
  | char    %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LChar $ charData $1) }
  | true    %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LBool $ strV $1) }
  | false   %shift { Src.Source (tokenArea $1) (tokenTarget $1) (Src.LBool $ strV $1) }
  | '{' '}' %shift { Src.Source (mergeAreas (tokenArea $1) (tokenArea $2)) (tokenTarget $1) Src.LUnit }

argsWithPlaceholder :: { [Src.Exp] }
  : '$' maybeComma                                     %shift { [Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "$")] }
  | '$' maybeComma 'ret'                               %shift { [Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "$")] }
  | '$' ',' argsWithPlaceholder maybeComma             %shift { (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "$")):$3 }
  | '$' 'ret' ',' argsWithPlaceholder maybeComma       %shift { (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "$")):$4 }
  | '$' ',' 'ret' argsWithPlaceholder maybeComma       %shift { (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "$")):$4 }
  | '$' 'ret' ',' 'ret' argsWithPlaceholder maybeComma %shift { (Src.Source (tokenArea $1) (tokenTarget $1) (Src.Var "$")):$5 }

  | exp ',' argsWithPlaceholder maybeComma             %shift { $1:$3 }
  | exp 'ret' ',' argsWithPlaceholder maybeComma       %shift { $1:$4 }
  | exp ',' 'ret' argsWithPlaceholder maybeComma       %shift { $1:$4 }
  | exp 'ret' ',' 'ret' argsWithPlaceholder maybeComma %shift { $1:$5 }
  | exp maybeComma                                     %shift { [$1] }
  | exp maybeComma 'ret'                               %shift { [$1] }

args :: { [Src.Exp] }
  : exp ',' args maybeComma             %shift { $1:$3 }
  | exp 'ret' ',' args maybeComma       %shift { $1:$4 }
  | exp ',' 'ret' args maybeComma       %shift { $1:$4 }
  | exp 'ret' ',' 'ret' args maybeComma %shift { $1:$5 }
  | exp maybeComma                      %shift { [$1] }
  | exp maybeComma 'ret'                %shift { [$1] }

param :: { Src.Source Name }
  : name %shift { Src.Source (tokenArea $1) (tokenTarget $1) (strV $1) }

params :: { [Src.Source Name] }
  : name ',' params %shift { Src.Source (tokenArea $1) (tokenTarget $1) (strV $1) : $3 }
  | name ',' name   %shift { [Src.Source (tokenArea $1) (tokenTarget $1) (strV $1), Src.Source (tokenArea $3) (tokenTarget $3) (strV $3)] }

{
buildPipe :: Area -> Src.SourceTarget -> [Src.Exp] -> Src.Exp
buildPipe area sourceTarget exps = Src.Source area sourceTarget (Src.Pipe exps)


buildAbsOrParenthesizedName :: Area -> Area -> Src.SourceTarget -> Src.Source Name -> Maybe (Bool, Area, [Src.Exp]) -> Src.Exp
buildAbsOrParenthesizedName parenLeftArea parenRightArea sourceTarget n@(Src.Source nameArea _ name) maybeExp = case maybeExp of
  Nothing ->
    Src.Source (mergeAreas parenLeftArea parenRightArea) sourceTarget (Src.Parenthesized parenLeftArea (Src.Source nameArea sourceTarget $ Src.Var name) parenRightArea)

  Just (False, _, [exp]) ->
    Src.Source (mergeAreas parenLeftArea (Src.getArea exp)) sourceTarget (Src.Abs [n] [exp])

  Just (True, initialArea, [exp]) ->
    Src.Source (mergeAreas parenLeftArea initialArea) sourceTarget (Src.AbsWithMultilineBody [n] [exp])

  Just (_, initialArea, exps) ->
    Src.Source (mergeAreas parenLeftArea initialArea) sourceTarget (Src.AbsWithMultilineBody [n] exps)


nameToPattern :: Area -> Src.SourceTarget -> String -> Src.Pattern
nameToPattern area sourceTarget n
  | n == "_"           = Src.Source area sourceTarget Src.PAny
  | (isUpper . head) n = Src.Source area sourceTarget (Src.PNullaryCon (Src.Source area sourceTarget n))
  | otherwise          = Src.Source area sourceTarget (Src.PVar n)


access :: Src.Exp -> Src.Exp -> Src.Exp
access src field = 
  Src.Source (mergeAreas (Src.getArea src) (Src.getArea field)) (Src.getSourceTarget src) (Src.Access src field)


sanitizeImportPath :: String -> String
sanitizeImportPath path =
  if length path > 1 then
    init $ tail path
  else
    path


lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap f = alexMonadScan >>= f


parseError :: Token -> Alex a
parseError (Token (Area (Loc a l c) _) _ cls) =
  alexError (printf "%d\n%d\nSyntax error - line: %d, column: %d\nThe following token is not valid: %s" l c l c (show cls))


parse :: String -> Either String Src.AST
parse s = runAlex s parseMadlib

parseForFormatter :: String -> Either String Src.AST
parseForFormatter s = runAlex s $ do
  enableFormatterMode
  parseMadlib
}
