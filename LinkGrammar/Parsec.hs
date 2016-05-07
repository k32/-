module LinkGrammar.Parsec
    (
     parseLink
    ) where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Data.List
import Data.Either

import LinkGrammar.AST

parseLink :: String -> Either String [Rule]
parseLink s = f $ parse (T.whiteSpace linkGrammarDef *> linkGrammar <* eof) "undefined" s
    where f (Right a) = Right a
          f (Left a)  = Left $ printError s a

printError :: String -> ParseError -> String
printError s e =
    let
        line = sourceLine $ errorPos e
        column = sourceColumn $ errorPos e
        range = 5
        sl = lines s
        dropN = maximum [0, line - range]
        splitN = minimum [range, line]
        (b, a) = splitAt splitN $ drop dropN sl
        moveSrcBy = 3
        moveSrc = map (replicate moveSrcBy ' ' ++)
    in
      unlines $ concat [ ["\nError: " ++ show e]
                       , ["\n..."]
                       , moveSrc b
                       , [(replicate (column - 1 + moveSrcBy) '~') ++ "^"]
                       , moveSrc $ take range a
                       , ["..."]
                       ]
    
linkGrammarDef :: (Monad m) => T.GenTokenParser String u m
linkGrammarDef = T.makeTokenParser $ T.LanguageDef {
                   T.commentStart = ""
                 , T.commentEnd = ""
                 , T.commentLine = "%"
                 , T.nestedComments = False
                 , T.opStart = oneOf $ nub $ concat ops
                 , T.opLetter = oneOf $ nub $ concat ops
                 , T.reservedNames = ["or"]
                 , T.reservedOpNames = ops
                 , T.caseSensitive = True
                 , T.identStart = anyToken
                 , T.identLetter = anyToken
                 }
    where ops = ["@", "+", "-"]

rOp, rW :: Monad m => String -> ParsecT String u m String
rOp = T.symbol linkGrammarDef -- T.reservedOp linkGrammarDef
rW = T.symbol linkGrammarDef -- T.reserved linkGrammarDef

tok :: Monad m => ParsecT String u m a -> ParsecT String u m a
tok = T.lexeme linkGrammarDef

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens = T.parens linkGrammarDef

linkGrammar :: Monad m => ParsecT String u  m [Rule]
linkGrammar = many rule

rule :: Monad m => ParsecT String u m Rule
rule = Rule <$> (many lval <* tok (char ':'))
            <*> (link      <* tok (char ';'))

lval :: Monad m => ParsecT String u m LVal
lval = choice [ try $ MacroDef <$> tok macroName
              , RuleDef  <$> tok nlpw
              ]

link :: Monad m => ParsecT String u m Link
link = (list $ LinkOr 0) <$> link' `sepBy` rW "or"
    where andLink = (rOp "&" <|> rW "and")

          link' = (list $ LinkAnd 0) <$> link'' `sepBy` andLink

          link'' = choice [ try $ (single $ Cost 1)           <$> T.squares linkGrammarDef link
                                                              <*  optional (T.float linkGrammarDef)
                          , try $ (single $ Optional 0)       <$> T.braces linkGrammarDef link
                          , try $ (none $ Link 0)             <$> (LinkID <$> linkName
                                                              <*> linkDirection)
                          , try $ (none Macro)                <$> macroName
                          , try $ (single $ MultiConnector 0) <$> (rOp "@" *> link'')
                          , try $ parens link
                          -- Empty links
                          , try $ rOp "[" *> rOp "]" *> pure (Node (Cost 1) [Node EmptyLink []])
                          , rOp "(" *> rOp ")"       *> pure (Node EmptyLink [])
                          ]

          none c x = Node (c x) []

          single c x = Node c [x]
                   
          list _ [a] = a
          list f  a  = Node f a

macroName :: Monad m => ParsecT String u m MacroName
macroName = tok $ between (char '<') (char '>') $ many1 (alphaNum <|> oneOf ",-.")

linkDirection :: Monad m => ParsecT String u m LinkDirection
linkDirection = choice [ rOp "+" *> pure Plus
                       , rOp "-" *> pure Minus
                       ]

nlpw :: Monad m => ParsecT String u m NLPWord
nlpw = tok $ NLPWord <$> nword <*> nclass
    where nword = choice [ try $ T.stringLiteral linkGrammarDef
                         , many1 $ oneOf "-=_," <|> alphaNum
                         ]
          nclass = choice [ try $ char '.' *> many1 (lower <|> digit <|> oneOf "=")
                          , pure ""
                          ]

linkName :: Monad m => ParsecT String u m LinkName
linkName = tok $ (++) <$> many1 upper <*> many (lower <|> digit <|> char '*')
