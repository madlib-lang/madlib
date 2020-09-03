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
  int   { Token _ (TokenInt $$) }
  str   { Token _ (TokenStr $$) }
  var   { Token _ (TokenVar $$) }
  '='   { Token _ TokenEq }
  '\n'  { Token _ TokenReturn }
  if    { Token _ TokenIf }
  '{'   { Token _ TokenLeftCurly }
  '}'   { Token _ TokenRightCurly }
  '('   { Token _ TokenLeftParen }
  ')'   { Token _ TokenRightParen }
  '===' { Token _ TokenTripleEq }
  false { Token _ (TokenBool $$) }
  true  { Token _ (TokenBool $$) }
%%

exps :: { [Exp] }
  : exp exps { $1 : $2 }
  | exp      { [$1] }

exp :: { Exp }
  : const var '=' term                         { AssignmentExpression (tokenToPos $1) $2 $4 }
  | if '(' literal operator literal ')' '{''}' { ConditionExpression (tokenToPos $1) (Cond $3 $4 $5) }

term :: { Term }
  : literal operator term    { BinaryTerm $1 $2 $3 }
  | literal                  { UnaryTerm $1 }

literal :: { Literal }
  : int   { IntLiteral $1 }
  | str   { StringLiteral $1 }
  | false { BoolLiteral $1 }
  | true  { BoolLiteral $1 }

operator :: { Operator }
  : '===' { TripleEq }

{

data Exp = AssignmentExpression TokenPos String Term
         | ConditionExpression TokenPos Cond
  deriving(Eq, Show)

data Cond = Cond Literal Operator Literal deriving(Eq, Show)

data Term = BinaryTerm Literal Operator Term 
          | UnaryTerm Literal
  deriving(Eq, Show)

data Literal = IntLiteral Int
             | StringLiteral String
             | BoolLiteral Bool
  deriving(Eq, Show)

data Operator = TripleEq deriving(Eq, Show)

lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap cont = do
    token <- alexMonadScan
    cont token

parseError :: Token -> Alex a
parseError (Token (TokenPos a l c) cls) = alexError (printf "Syntax error - line: %d, column: %d\nThe following token is not valid: %s" l c (show cls))

parse :: String -> Either String [Exp]
parse s = runAlex s parseMadlib
}
