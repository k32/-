module LinkGrammar.Process
    (
      normalize
    , makeRuleset
  --  , containsID
    , Ruleset(..)
    ) where

import LinkGrammar.AST 
import Data.PrettyPrint
import Data.Tree
import qualified Data.Map as M
    
data Ruleset = Ruleset {
      _rules :: [Rule]
    , _macros :: M.Map MacroName Link
    }
    deriving (Show, Eq, Read)

instance PrettyPrint Ruleset where
    pretty (Ruleset l) = unlines $ map pretty l
             
makeRuleset :: [Rule] -> Ruleset
makeRuleset rules = Ruleset {
                      _rules = rules
                    , _macros = undefined
                    }
    
normalize :: Ruleset -> Ruleset
normalize (Ruleset l) = Ruleset $ map (\(Rule a b) -> Rule a $ flattenAssoc b) l

-- TODO : write me, since probability is affected by the definition order            
flattenAssoc :: Link -> Link
flattenAssoc = id

(=*=) :: LinkID -> LinkID -> Bool
(LinkID x _) =*= (LinkID y _) = f x y
    where
     f [] _                   = True
     f _  []                  = True
     f (a:t1) (b:t2)
         | a == b             = f t1 t2
         | any (=='*') [a, b] = True

-- containsID :: LinkID -> Link -> Bool
-- containsID i (Link l)           = i =*= l
-- containsID i (LinkOr l)         = any (containsID i) l
-- containsID i (LinkAnd l)        = any (containsID i) l
-- containsID i (Optional l)       = containsID i l
-- containsID i (MultiConnector l) = containsID i l
-- containsID i (Cost l)           = containsID i l
-- containsID _ EmptyLink          = False
