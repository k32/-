{
module LinkGrammar.Parser (parse) where
import LinkGrammar.Tokenize
import LinkGrammar.AST
}

%name parse
-- %lexer { happyLexer } { TEOF }
%monad { Either String }
%tokentype { Token }
%error { parseError }
%token term         { TTerm $$ }
       macro        { TRuleId $$ }
       link         { TLinkId $$ }
       ':'          { TColon }
       ';'          { TSemicolon }
       '{'          { TLBrace }
       '}'          { TRBrace }
       '('          { TLParen }
       ')'          { TRParen }
       '['          { TLSqbr }
       ']'          { TRSqbr }
       or           { TOr }
       '&'          { TAmpersand }
       '+'          { TPlus }
       '-'          { TMinus }
       number       { TNumber $$ }
       '@'          { TAt }
       eof          { TEOF }

%left '@'
%right '+' '-'
%left '&'
%left or


%%
Rules           : Rule eof                  { [$1] }
                | Rule Rules                { $1 : $2 }

Rule            : LVal ':' Link ';'         { Rule $1 $3 }

Words           : term                      { [NLPWord $1 ""] }
                | term Words                { (NLPWord $1 "") : $2 }

Macros          : macro                     { [$1] }
                | macro Macros              { $1 : $2 }

LVal            : Macros                    { MacroDef $1 }
                | Words                     { RuleDef $1 }

LinkDirection   : '+'                       { Plus }
                | '-'                       { Minus }

Link            : macro                     { Macro $1 }
                | '(' ')'                   { Empty }
                | '[' ']'                   { Empty } -- TODO that is wrong!
                | link LinkDirection        { Link $1 $2 }
                | '@' Link                  { MultiConnector $2 }
                | Link or Link              { $1 :|: $3 }
                | Link '&' Link             { $1 :&: $3 }
                | '[' Link ']'              { Cost $2 }
                | '{' Link '}'              { Optional $2 }
                | '(' Link ')'              { $2 }

{
parseError :: [Token] -> Either String a
parseError a = Left $ "Parsing error near " ++  show (take 100 a)
}
