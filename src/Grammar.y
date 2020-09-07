{
module Grammar where
import Lexer
import Text.Printf
}

%name parseMadlib
%tokentype { Token }
%error { parseError }
%monad {Alex}
%lexer {lexerWrap} {Token _ TokenEOF}

%token
  const { Token _ TokenConst }
  int   { Token _ (TokenInt _) }
  str   { Token _ (TokenStr _) }
  name  { Token _ (TokenName _) }
  '='   { Token _ TokenEq }
  '+'   { Token _ TokenPlus }
  '::'  { Token _ TokenDoubleColon }
  '->'  { Token _ TokenArrow }
  '=>'  { Token _ TokenFatArrow }
  '\n'  { Token _ TokenReturn }
  if    { Token _ TokenIf }
  ','   { Token _ TokenComa }
  '{'   { Token _ TokenLeftCurly }
  '}'   { Token _ TokenRightCurly }
  '('   { Token _ TokenLeftParen }
  ')'   { Token _ TokenRightParen }
  '===' { Token _ TokenTripleEq }
  false { Token _ (TokenBool _) }
  true  { Token _ (TokenBool _) }
%%

exps :: { [Exp] }
  : exp exps            { $1 : $2 }
  | exp                 { [$1] }

exp :: { Exp }
  : const name '=' term                       { AssignmentExpression (tokenToPos $1) (strV $2) $4 }
  | if '(' term ')' '{' '}'                   { ConditionExpression (tokenToPos $1) (Cond $3) }
  | name '::' types                           { Typing (tokenToPos $1) (strV $1) $3 }
  | name '=' '(' params ')' '=>' body         { FunctionDeclaration (tokenToPos $1) (strV $1) $4 $7 }

types :: { [Type] }
  : name '->' types { Type (strV $1) : $3 }
  | name            { [Type (strV $1)] }

params :: { [Param] }
  : name ',' params { Param (strV $1) : $3 }
  | name            { [Param (strV $1)] }

body :: { Body }
  : term { Body $1 }

-- Make this an array
term :: { Term }
  : value operator term    { BinaryTerm $1 $2 $3 }
  | value                  { UnaryTerm $1 }

value :: { Value }
  : literal { LiteralValue $1 }
  | name    { Variable (strV $1) }

literal :: { Literal }
  : int   { IntLiteral $ intV $1 }
  | str   { StringLiteral $ strV $1 }
  | false { BoolLiteral $ boolV $1 }
  | true  { BoolLiteral $ boolV $1 }

operator :: { Operator }
  : '===' { TripleEq }
  | '+'   { Plus }

{

data Exp = AssignmentExpression TokenPos String Term
         | ConditionExpression TokenPos Cond
         | Typing TokenPos String [Type]
         | FunctionDeclaration TokenPos String [Param] Body
  deriving(Eq, Show)

data Cond = Cond Term deriving(Eq, Show)

data Type = Type String deriving(Eq, Show)

data Param = Param String deriving(Eq, Show)

data Body = Body Term deriving(Eq, Show)

data Term = BinaryTerm Value Operator Term 
          | UnaryTerm Value
  deriving(Eq, Show)

data Value = LiteralValue Literal | Variable String deriving(Eq, Show)

data Literal = IntLiteral Int
             | StringLiteral String
             | BoolLiteral Bool
  deriving(Eq, Show)

data Operator = TripleEq 
              | Plus
  deriving(Eq, Show)

lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap cont = do
    token <- alexMonadScan
    cont token

parseError :: Token -> Alex a
parseError (Token (TokenPos a l c) cls) = alexError (printf "Syntax error - line: %d, column: %d\nThe following token is not valid: %s" l c (show cls))

parse :: String -> Either String [Exp]
parse s = runAlex s parseMadlib
}
