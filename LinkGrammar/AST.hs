{-# LANGUAGE FlexibleInstances #-}
module LinkGrammar.AST
  (
    Link(..)
  , LinkID(..)
  , NodeType(..)
  , Rule(..)
  , LinkName, MacroName
  , NLPWord(..)
  , LinkDirection(..)
  , LVal(..)
  , module Data.Tree
  )
  where

import Data.PrettyPrint    
import Data.List
import Data.Tree
import Data.Tree.Zipper
   
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

instance PrettyPrint LinkID where
    pretty (LinkID a b) =  a ++ pretty b
            
data NodeType = Optional
              | Link LinkID
              | LinkAnd
              | LinkOr
              | Macro MacroName
              | MultiConnector
              | Cost
              | EmptyLink
              deriving (Eq, Show)

type Link = Tree NodeType

type Link' t = TreePos t NodeType

paren :: Link -> String
paren a@Node {rootLabel = r} =
    case r of
      Link _   -> pretty a
      Macro _  -> pretty a
      Optional -> pretty a
      Cost     -> pretty a
      _        -> "(" ++ pretty a ++ ")"

instance PrettyPrint Link where
    pretty Node {rootLabel = r, subForest = l} = 
        case r of
          Link a         -> pretty a
          Macro a        -> "<" ++ a ++  ">"
          LinkOr         -> intercalate " or " (map paren l)
          LinkAnd        -> intercalate " & " (map paren l)
          Optional       -> "{ " ++ pretty (head l) ++ " }"
          MultiConnector -> "@" ++ paren (head l)
          Cost           -> "[ " ++ pretty (head l) ++ " ]"
          EmptyLink      -> "()"

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
