{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module LinkGrammar.AST
  (
    Link(..)
 -- , Link'
  , LinkID(..)
  , NodeType(..)
  , Rule(..)
  , LinkName, MacroName
  , NLPWord(..)
  , LinkDirection(..)
  , LVal(..)
  , exactCompare
  , module Data.Tree
--  , module Data.Tree.Zipper
  )
  where

import Data.PrettyPrint
import Data.List
import Data.Tree
import Data.Binary
import GHC.Generics (Generic)

data LinkDirection = Plus
                   | Minus
                   deriving (Eq, Show, Generic, Ord)

instance Binary LinkDirection                            

instance PrettyPrint LinkDirection where
  pretty Plus  = "+"
  pretty Minus = "-"

instance Show LinkID where
    show (LinkID a b) = a ++ (pretty b)

type LinkName = String

type MacroName = String

data NLPWord =
    NLPWord
    {
      _nlpword
    , _nlpclass :: String
    }
    deriving (Eq, Generic, Show)

instance Binary NLPWord

instance PrettyPrint NLPWord where
    pretty NLPWord {_nlpword = w, _nlpclass = c}
        | null c = w
        | True = w ++ ".[" ++ c ++ "]"

data LinkID = LinkID {
      _linkName :: LinkName
    , _linkDirection :: LinkDirection
    } deriving (Eq, Generic)

instance Binary LinkID

instance Ord LinkID where
    compare (LinkID k i) (LinkID l j) =
        case compare i j of
          EQ ->
              go k l where
                    go [] _ = EQ
                    go _ [] = EQ
                    go (a:t1) (b:t2)
                        | a == b || any (=='*') [a,b] = go t1 t2
                        | True                        = compare a b
          a ->
              a

exactCompare :: LinkID -> LinkID -> Ordering
exactCompare (LinkID k i) (LinkID l j) =
    case compare i j of
      EQ -> compare k l
      a  -> a

instance PrettyPrint LinkID where
    pretty (LinkID a Plus) =  a ++ "+"
    pretty (LinkID a Minus) = a ++ "-"

data NodeType = Optional
              | Link Float LinkID
              | LinkAnd
              | LinkOr
              | Macro MacroName
              | MultiConnector
              | Cost Float
              | EmptyLink
              deriving (Eq, Show, Generic)

instance Binary NodeType

type Link = Tree NodeType

-- type Link' t = TreePos t NodeType

paren :: Link -> String
paren a@Node {rootLabel = r} =
    case r of
      Link _ _ -> pretty a
      Macro _  -> pretty a
      Optional -> pretty a
      Cost _   -> pretty a
      _        -> "(" ++ pretty a ++ ")"

instance PrettyPrint Link where
    pretty Node {rootLabel = r, subForest = l} =
        case r of
          Link _ a       -> pretty a
          Macro a        -> "<" ++ a ++  ">"
          LinkOr         -> intercalate " or " (map paren l)
          LinkAnd        -> intercalate " & " (map paren l)
          Optional       -> "{ " ++ pretty (head l) ++ " }"
          MultiConnector -> "@" ++ paren (head l)
          Cost x
              | x /= 1   -> "[ " ++ pretty (head l) ++ " ]" ++ show x ++ " "
              | True     -> "[ " ++ pretty (head l) ++ " ]"
          EmptyLink      -> "()"

data LVal = RuleDef { _ruleDef :: NLPWord }
          | MacroDef { _macroName :: MacroName }
          deriving (Eq, Show, Generic)

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
