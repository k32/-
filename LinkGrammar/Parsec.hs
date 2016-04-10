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
parseLink = f . parse (T.whiteSpace linkGrammarDef *> linkGrammar <* eof) "undefined"
    where f (Right a) = Right a
          f (Left a)  = Left $ show a

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
                 , T.identStart = undefined
                 , T.identLetter = undefined
                 }
    where ops = ["@", ":", ";", "+", "-"]

rOp, rW :: Monad m => String -> ParsecT String u m ()
rOp = T.reservedOp linkGrammarDef
rW = T.reserved linkGrammarDef

tok :: Monad m => ParsecT String u m a -> ParsecT String u m a
tok = T.lexeme linkGrammarDef

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens = T.parens linkGrammarDef

linkGrammar :: Monad m => ParsecT String u  m [Rule]
linkGrammar = many rule

rule :: Monad m => ParsecT String u m Rule
rule = Rule <$> (many1 lval <* rOp ":")
            <*> (link       <* rOp ";")

lval :: Monad m => ParsecT String u m LVal
lval = choice [ try $ MacroDef <$> tok macroName
              , RuleDef  <$> tok nlpw
              ]

-- a ~> b = (rOp a, b)
       
-- infixOps :: Monad m
--          => [[(ParsecT String u m x, a -> a -> a)]]
--          -> ParsecT String u m a
--          -> ParsecT String u m a
-- infixOps 

link :: Monad m => ParsecT String u m Link
link = choice [ try $ (:|:) <$> (link' <* rW "or") <*> link
              , link'
              ]
    where link' = choice [ try $ (:&:) <$> (link'' <* rOp "&") <*> link
                         , link''
                         ]
          link'' = choice [ try $ parens link
                          , try $ Link  <$> linkName <*> linkDirection
                          ,       Macro <$> macroName
                          ]

macroName :: Monad m => ParsecT String u m MacroName
macroName = tok $ between (char '<') (char '>') $ many1 alphaNum

linkDirection :: Monad m => ParsecT String u m LinkDirection
linkDirection = choice [ rOp "+" *> pure Plus
                       , rOp "-" *> pure Minus
                       ]

nlpw :: Monad m => ParsecT String u m NLPWord
nlpw = tok $ NLPWord <$> nword <*> nclass
    where nword = choice [ try $ T.stringLiteral linkGrammarDef
                         , many1 $ oneOf "-=." <|> lower
                         ]
          nclass = choice [ try $ char '.' *> many1 (lower <|> oneOf "=") -- TODO:
                                       -- this never works
                          , pure ""
                          ]

linkName :: Monad m => ParsecT String u m LinkName
linkName = tok $ (++) <$> many1 upper <*> many (lower <|> digit <|> char '*')
