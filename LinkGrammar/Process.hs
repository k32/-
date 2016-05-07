{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveGeneric, TemplateHaskell #-}
module LinkGrammar.Process
    (
      makeRuleset
    , Ruleset(..)
    , Rule'(..)
    , saveRuleset
    , loadRuleset
    ) where

import Control.Lens
import LinkGrammar.AST
import Data.PrettyPrint
import Data.Tree
import Data.List
import Data.Traversable
import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader    
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Binary
import Data.TernaryTree as TT
import GHC.Generics

type RuleIndex = [(Int, [Int])]
    
data Rule' = Rule' {
      _lval' :: ![NLPWord]
    , _links' :: !Link
    } deriving (Show, Eq, Generic)
makeLenses ''Rule'
instance Binary Rule'

data Ruleset = Ruleset {
      _rules :: V.Vector Rule'
    , _uplinks
    , _downlinks :: TTree Char RuleIndex
    }  deriving (Show)
makeLenses ''Ruleset

makeRuleset :: [Rule] -> Ruleset
makeRuleset rr =
    let
        (macros, rules) = sortOut rr

        rules' = V.fromList $ map (assocFlatten . costPropagate . deMacrify macros [])
                 rules

        ruleset0 = Ruleset undefined TT.empty TT.empty
        ruleset' = (`execState` ruleset0) $ V.imapM_ makeIndex rules'

        makeIndex idx (Rule' _ y) = (`runReaderT` []) $ go idx y

        go :: (MonadReader [Int] m, MonadState Ruleset m) =>
              Int -> Link -> m ()
        go idx Node {subForest = u, rootLabel = p}
            = case p of
                Link _ (LinkID li ld) -> do
                       path <- ask
                       let setter = case ld of
                                      Plus -> uplinks
                                      Minus -> downlinks
                       setter %= TT.insertWith (++) li [(idx, reverse $ idx:path)]
                _ ->
                  mapM_ (\(s, n) -> local (n:) $ go idx s) $ zip u [0..]
        
    in
      ruleset' {
          _rules = rules'
        }

saveRuleset :: Ruleset -> FilePath -> IO ()
saveRuleset Ruleset{_rules=r, _uplinks=u, _downlinks=d} fileName = encodeFile fileName (V.toList r, u, d)

loadRuleset :: FilePath -> IO Ruleset
loadRuleset filePath = do
  (r, u, d) <- decodeFile filePath
  return Ruleset {_rules=V.fromList r, _uplinks=u, _downlinks=d}
     

(=*=) :: LinkID -> LinkID -> Bool
(LinkID x _) =*= (LinkID y _) = f x y
    where
     f [] _                   = True
     f _  []                  = True
     f (a:t₁) (b:t₂)
         | a == b             = f t₁ t₂
         | any (=='*') [a, b] = True

sortOut :: [Rule] -> (M.Map MacroName Link, [Rule'])
sortOut = foldl f (M.empty, [])
    where f (m, a) Rule {_lval = l, _links = r} =
              (
                foldr (\x -> M.insert x r) m m1
              , a1 : a
              )
             where 
               (m1, a1) = (map _macroName *** rule') $ partition isMacro l

               rule' = ((flip Rule') r) . map _ruleDef

          isMacro (RuleDef _) = False
          isMacro _           = True

          split = partition isMacro

deMacrify :: M.Map MacroName Link -> [MacroName] -> Rule' -> Rule'
deMacrify m l (Rule' ł r) =
    let
        f :: [MacroName] -> Link -> State [MacroName] Link
        f l₀ (Node α β) = case α of
                            Macro n -> do
                                if n `elem` l₀
                                   then error $ "deMacrify: Loop detected, see macro " ++ n ++
                                                " in rule " ++ show ł
                                   else return ()
                                modify (n:) -- TODO: duplicates
                                return $ m M.! n
                            _ -> do
                                c' <- mapM (f l₀) β
                                return Node {
                                             rootLabel = α
                                           , subForest = c'
                                           }
        (r', l₁) = (`runState` l) $ f l r
    in
      if r' == r
         then (Rule' ł r')
         else deMacrify m l₁ (Rule' ł r')

costPropagate :: Rule' -> Rule'
costPropagate (Rule' lval links) = Rule' lval $ go 0 links
  where go n node@Node{subForest=s, rootLabel=l} =
            case l of
              Cost x ->
                  go (n+x) $ head s
              (Link cost linkID) ->
                  Node {rootLabel=Link (cost + n) linkID, subForest=[]}
              (LinkOr cost) ->
                  Node {
                     rootLabel = LinkOr $ cost+n
                   , subForest = map (go n) s
                   }
              (LinkAnd cost) ->
                  Node {
                     rootLabel = LinkAnd $ cost+n
                   , subForest = map (go n) s
                   }
              (Optional cost) ->
                  Node {
                     rootLabel = Optional $ cost+n
                   , subForest = map (go n) s
                   }
              (MultiConnector cost) ->
                  Node {
                     rootLabel = MultiConnector $ cost+n
                   , subForest = map (go n) s
                   }


{-
a or (b or c) or d -> a or b or c or d

Note: we don't flatten &, because it's not needed
-}               
assocFlatten :: Rule' -> Rule'
assocFlatten = id -- (Rule' lval links) =  Rule' lval $ go links
  -- where go node@Node{subForest=s, rootLabel=l} =
  --           case l of
  --             LinkOr ->
  --                 let

  --                     s' = foldl f
