{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module LinkGrammar.AST (
    Link(..)
  , Rule(..)
  , LinkName, MacroName
  , NLPWord(..)
  , LinkDirection(..)
  , LVal(..)
  ) where

data LinkDirection = Plus
                   | Minus
                   deriving (Eq)

instance Show LinkDirection where
  show Plus  = "+"
  show Minus = "-"

type LinkName = String
-- instance ShowPrint LinkName where
--   show = show

type MacroName = String

data NLPWord = NLPWord
               {
                 _nlpword
               , _nlpclass :: String
               }
               deriving (Eq)

instance Show NLPWord where
  show (NLPWord a b) | null b = a
                     | True   = a ++ "." ++ b

data Link = Link LinkName LinkDirection
          | Macro MacroName
          | Link :|: Link
          | Link :&: Link
          | Optional Link
          | MultiConnector Link
          | Cost Link
          | Empty
          deriving (Eq)

paren :: Link -> String
paren (Link a b) = a ++ show b
paren (Macro a) = show a
paren a = "(" ++ show a ++ ")"

instance Show Link where
  show (Link a b) = a ++ (show b)
  show (Macro a) = "<" ++ a ++  ">"
  show (a :|: b) = paren a ++ " or " ++ paren b
  show (a :&: b) = paren a ++ " & " ++ paren b
  show (Optional a) = "{ " ++ show a ++ " }"
  show (MultiConnector a) = "@" ++ paren a
  show (Cost a) = "[ " ++ show a ++ " ]"
  show Empty = "()"

data LVal = RuleDef [NLPWord]
          | MacroDef [MacroName]
          deriving (Eq)

instance Show LVal where
  show (RuleDef a) = unwords $ map show a
  show (MacroDef a) = show a

data Rule = Rule
            {
              _lval  :: LVal
            , _links :: Link
            }
            deriving (Eq)

instance Show Rule where
  show (Rule a b) = show a ++ " : " ++ show b ++ " ; "

