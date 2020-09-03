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
%%

exps :: {[Exp]}
  : exp exps { $1 : $2 }
  | exp      { [$1] }

exp :: {Exp}
  : const var '=' literal { AssignmentExpression (tokenToPos $1) $2 $4 }

literal :: { Literal }
  : int { Int $1 }
  | str { String $1 }

{
data Exp = AssignmentExpression TokenPos String Literal
  deriving(Eq, Show)

data Literal = Int Int
             | String String
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
