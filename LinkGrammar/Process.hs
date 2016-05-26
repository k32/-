{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveGeneric, TemplateHaskell #-}
module LinkGrammar.Process
    (
      makeRuleset
    , Ruleset(..)
    , Rule'(..)
    , withRuleset
    ) where

import Control.Lens
import LinkGrammar.AST
import Data.PrettyPrint
import Data.Tree
import Data.List
import qualified Data.Set as S
import Data.Traversable
import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.Reader    
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Binary (encode, encodeFile, decodeFile, Binary)
import Data.ByteString.Lazy (hPut)
import Data.TernaryTree as TT
import GHC.Generics
import System.IO (Handle, hTell, withFile, IOMode(..))
import Control.DeepSeq
import Debug.Trace

type Offset = Integer

type RuleIndex = S.Set Offset
    
data Rule' = Rule' {
      _lval' :: ![NLPWord]
    , _links' :: !Link
    } deriving (Show, Eq, Generic)
makeLenses ''Rule'
instance Binary Rule'
instance NFData Rule'

data Ruleset r = Ruleset {
      _rules :: r
    , _uplinks
    , _downlinks :: {- (M.Map String RuleIndex) -}
                    !(TTree Char RuleIndex)
    }  deriving (Generic, Show)
makeLenses ''Ruleset
instance (Binary a) => Binary (Ruleset a)

-- type MakeRulesetState = (M.Map String RuleIndex, M.Map String RuleIndex)
type MakeRulesetState = (TT.TTree Char RuleIndex, TT.TTree Char RuleIndex)

makeRuleset :: String -> [Rule] -> IO ()
makeRuleset outfile rr =
    let
       -- ruleset0 = Ruleset {_rules=(), _uplinks=TT.empty, _downlinks=TT.empty}
        ruleset0 = (TT.empty, TT.empty)
        -- ruleset0 = (M.empty, M.empty)
        
        (macros, rules) = sortOut rr

        rules' = -- map (assocFlatten . costPropagate)
                 rules

        writeRule handle r = liftIO $ do
          -- putStrLn "Writing rule..."
          hPut handle $ encode r
          -- putStrLn "Done..."

        dumpRule :: (MonadState MakeRulesetState m, MonadIO m) => Handle -> Rule' -> m ()
        dumpRule handle rule = do
          pos <- liftIO $ hTell handle
          writeRule handle rule
          (`runReaderT` []) $ go pos $ _links' rule

        go :: (MonadReader [Int] m, MonadState MakeRulesetState m) =>
              Offset -> Link -> m ()
        go offset Node {subForest = u, rootLabel = p}
            = case p of
                Link _ (LinkID li ld) -> do
                       path <- ask
                       let setter = case ld of
                                      Plus -> _1
                                      Minus -> _2
                       setter %= TT.insertWith (S.union) li (S.singleton offset)
                Macro m ->
                  go offset $ macros M.! m
                _ ->
                  mapM_ (\(s, n) -> local (n:) $ go offset s) $ zip u [0..]
        
    in do
      putStrLn "Dumping macros..."
      encodeFile (outfile ++ ".macros") $ M.toList macros
      putStrLn "Done."
      withFile (outfile ++ ".rules") WriteMode $ \hRules -> do
        putStrLn "Dumping rules..."
        index <- (`execStateT` ruleset0) $ mapM (dumpRule hRules) rules'
        encodeFile (outfile ++ ".index") index

withRuleset :: FilePath -> (Ruleset Handle -> IO a) -> IO a
withRuleset filePath f = do
  ruleset <- (decodeFile $ filePath ++ ".idx") :: IO (Ruleset ())
  withFile (filePath ++ ".rules") ReadMode (\handle -> f $ ruleset{_rules=handle})
     
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

deMacrify :: M.Map MacroName Link -> Rule' -> Rule'
deMacrify m rule@(Rule' ł r) =
    let
        f :: Link -> Reader (S.Set MacroName) Link
        f (Node α β) =
          case α of
            Macro n
              | n `M.member` m -> do
                  l₀ <- ask
                  if n `S.member` l₀
                    then error $ "deMacrify: Loop detected, see macro <" ++ n ++
                                 "> in rule \"" ++ pretty r ++ "\"\nUsed macros: " ++
                                 unwords (S.toList l₀)
                    else return ()
                  local (n `S.insert`) $
                    f $ m M.! n
              | True ->
                  error $ "Undefined macro <" ++ n ++ "> in rule \"" ++ pretty r ++ "\""
            _ -> do
              c' <- mapM f β
              return $! Node {
                  rootLabel = α
                , subForest = c'
                }

        r' = (`runReader` S.empty) $ f r
    in
     Rule' ł r'

-- costPropagate :: Rule' -> Rule'
-- costPropagate (Rule' !lval !links) = Rule' lval $! go 0 links
--   where go n node@Node{subForest=s, rootLabel=l} =
--             case l of
--               Macro m ->
--                 Macro m {- TODO: this doesn't work -}
--               Cost x ->
--                 go (n+x) $! head s
--               (Link cost linkID) ->
--                 Node {rootLabel=Link (cost + n) linkID, subForest=[]}
--               (LinkOr cost) ->
--                 Node {
--                      rootLabel = LinkOr $! cost+n
--                    , subForest = map (go n) s
--                    }
--               (LinkAnd cost) ->
--                   Node {
--                      rootLabel = LinkAnd $! cost+n
--                    , subForest = map (go n) s
--                    }
--               (Optional cost) ->
--                   Node {
--                      rootLabel = Optional $! cost+n
--                    , subForest = map (go n) s
--                    }
--               (MultiConnector cost) ->
--                   Node {
--                      rootLabel = MultiConnector $! cost+n
--                    , subForest = map (go n) s
--                    }


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
