{
module Grammar where
import Lexer
}

%name parseExpressions exps
%tokentype { Token }
%error { parseError }

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

parseError :: [Token] -> a
parseError t = error $ "Parse error: " ++ show t

data Exp = AssignmentExpression TokenPos String Literal
    deriving(Eq, Show)

data Literal = Int Int
             | String String
    deriving(Eq, Show)
}
