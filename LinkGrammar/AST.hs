{-# LANGUAGE FlexibleInstances #-}
module LinkGrammar.AST
  -- (
  --   Link(..)
  -- , Rule(..)
  -- , LinkName, MacroName
  -- , NLPWord(..)
  -- , LinkDirection(..)
  -- , LVal(..)
  -- )
      where

import Data.PrettyPrint    
import Data.List
   
data LinkDirection = Plus
                   | Minus
                   deriving (Eq, Show, Read)

instance PrettyPrint LinkDirection where
  pretty Plus  = "+"
  pretty Minus = "-"

type LinkName = String
-- instance ShowPrint LinkName where
--   show = show

type MacroName = String

data NLPWord =
    NLPWord
    {
      _nlpword
    , _nlpclass :: String
    }
    deriving (Eq, Read, Show)

instance PrettyPrint NLPWord where
    pretty NLPWord {_nlpword = w, _nlpclass = c}
        | null c = w
        | True = w ++ ".[" ++ c ++ "]"
    
-- instance Show NLPWord where
--   show (NLPWord a b) | null b = a
--                      | True   = "\"" ++ a ++ "\"." ++ b

data LinkID = LinkID {
      _linkName :: LinkName
    , _linkDirection:: LinkDirection
    } deriving (Show, Eq, Read)

data Link = Link LinkID
          | Macro MacroName
          | LinkOr [Link]
          | Link :&: Link
          | Optional Link
          | MultiConnector Link
          | Cost Link
          | EmptyLink
          deriving (Eq, Show)

paren :: Link -> String
paren (Link (LinkID a b)) = a ++ pretty b
paren a@(Macro _)         = pretty a
paren a@(Optional _)      = pretty a
paren a@(Cost _)          = pretty a
paren a                   = "(" ++ pretty a ++ ")"

instance PrettyPrint Link where
    pretty (Link (LinkID a b)) = a ++ (pretty b)
    pretty (Macro a)           = "<" ++ a ++  ">"
    pretty (LinkOr a)          = intercalate " or " (map paren a)
    pretty (a :&: b)           = paren a ++ " & " ++ paren b
    pretty (Optional a)        = "{ " ++ pretty a ++ " }"
    pretty (MultiConnector a)  = "@" ++ paren a
    pretty (Cost a)            = "[ " ++ pretty a ++ " ]"
    pretty EmptyLink           = "()"

data LVal = RuleDef NLPWord
          | MacroDef MacroName
          deriving (Eq, Show)

instance PrettyPrint LVal where
  pretty (RuleDef a) = pretty a
  pretty (MacroDef a) = "<" ++ (pretty a) ++ ">"

data Rule = Rule
            {
              _lval  :: [LVal]
            , _links :: Link
            }
            deriving (Eq, Show)

instance PrettyPrint Rule where
    pretty (Rule a b) = pretty a ++ " : " ++ pretty b ++ " ; "

instance PrettyPrint [LVal] where
    pretty = unwords . map pretty
