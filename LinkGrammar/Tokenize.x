{
module LinkGrammar.Tokenize where
}

%wrapper "monadUserState"

$digit          = 0-9
$special        = [ \: \; \  \" \{ \} \< \> ]
$notspecial     = ~$special
$asclarge       = [A-Z]
$ascsmall       = [a-z]

@linkid         = $asclarge+ [$ascsmall $digit \*]*
@number         = ($digit+\.)?($digit+)
@terminal       = $notspecial+
@macroid        = \<$notspecial+\>
@quoted         = \" ([^\"]| \\ \")* \"

tokens :-
-- Drop whitespace everywhere
<0, links>   $white+                  ;
-- Skip comments 
<0,links>    "%".*                    ;
-- Colon separates terminals from links
<0>          \:                       { const' TColon `andBegin` links }
<links>      \;                       { const' TSemicolon `andBegin` 0 }
<links>      \(                       { const' TLParen }
<links>      \)                       { const' TRParen }
<links>      \{                       { const' TLBrace }
<links>      \}                       { const' TRBrace }
<links>      \[                       { const' TLSqbr }
<links>      \]                       { const' TRSqbr }
<links>      \+                       { const' TPlus }
<links>      \-                       { const' TMinus }
<links>      or                       { const' TOr }
-- TODO: Verify if it's ok:
<links>      and                      { const' TAmpersand }
<links>      \&                       { const' TAmpersand }
<links>      \@                       { const' TAt }
<links>      @linkid                  { tok TLinkId }
<0>          @terminal                { tok TTerm }
<0,links>    @macroid                 { tok (\s -> TRuleId $ init $ tail s) }
<0>          @quoted                  { tok (\s -> TTerm $ read s) }

{
data AlexUserState = UsrState {
                     }


alexInitUserState = UsrState {
                    }

alexEOF = return TEOF

const' f _ _ = return f

data Token = TTerm       String
           | TRuleId     String
           | TLinkId     String
           | TColon
           | TDot
           | TSemicolon
           | TLBrace
           | TRBrace
           | TLSqbr
           | TRSqbr
           | TOr
           | TAmpersand
           | TLParen
           | TRParen
           | TPlus
           | TMinus
           | TNumber     Float
           | TAt
           | TQuote
           | TEOF
           deriving (Eq)

instance Show Token where
  show (TTerm x) = "\"" ++ x ++ "\""
  show (TRuleId x) = "<" ++ x ++ ">"
  show (TLinkId x) = " l\"" ++ x ++ "\""
  show TColon = " : "
  show TSemicolon = " ; "
  show TLBrace = " { "
  show TRBrace = " } "
  show TLParen = " ( "
  show TRParen = " ) "
  show TOr = " or "
  show TAmpersand = " & "
  show TPlus = " + "
  show TMinus = " - "
  show (TNumber n) = show n
  show TAt = " @ "
  show TQuote = " \" "
  show TLSqbr = " [ "
  show TRSqbr = " ] "
  show TDot = "."
  show TEOF = "EOF"

tok f (_,_,_,s) l = return $ f (take l s)

tokenize str = runAlex str $ do
   let loop = do tok <- alexMonadScan
                 if tok == TEOF
                   then return [tok]
			             else do toks <- loop
				             return (tok:toks)
   loop  

happyLexer :: (Monad m) => (Token -> m a) -> m a
happyLexer str cont = runAlex 
}
