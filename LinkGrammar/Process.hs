{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveGeneric, TemplateHaskell #-}
module LinkGrammar.Process
    (
      makeRuleset
    , Ruleset(..), ruleset, uplinks, downlinks
    , Rule'(..)
    , Macros
    , RulesetIndex
    , Offset
    , (=*=)
    , (*<)
    , flipLink
    , deMacrify
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

type RuleIndex = S.Set Int

type Macros = M.Map MacroName Link
    
data Rule' = Rule' {
      _lval' :: ![NLPWord]
    , _links' :: !Link
    } deriving (Show, Eq, Generic)
makeLenses ''Rule'
instance Binary Rule'
instance NFData Rule'

data Ruleset r = Ruleset {
      _ruleset :: r
    , _uplinks
    , _downlinks :: {- (M.Map String RuleIndex) -}
                    !(TTree Char RuleIndex)
    }  deriving (Generic, Show)
makeLenses ''Ruleset
instance (Binary a) => Binary (Ruleset a)

type RulesetIndex = Ruleset [Offset]

makeRuleset :: String
            -> [Rule]
            -> IO ()
makeRuleset outfile rr =
    let
        ruleset0 = Ruleset {_ruleset=[], _uplinks=TT.empty, _downlinks=TT.empty}
        
        (macros, rules) = sortOut rr
       
        writeRule handle r = liftIO $ hPut handle $ encode r

        dumpRule :: (MonadState RulesetIndex m, MonadIO m) => Handle -> (Int, Rule') -> m ()
        dumpRule handle (myId, rule) = do
          pos <- liftIO $ hTell handle
          writeRule handle rule
          ruleset %= (pos:)
          (`runReaderT` []) $ go myId $ _links' rule

        go :: (MonadReader [Int] m, MonadState RulesetIndex m) =>
              Int -> Link -> m ()
        go myId Node {subForest = u, rootLabel = p}
            = case p of
                Link _ (LinkID li ld) -> do
                       path <- ask
                       let setter = case ld of
                                      Plus -> uplinks
                                      Minus -> downlinks
                       setter %= TT.insertWith (S.union) li (S.singleton myId)
                Macro m ->
                  go myId $ macros M.! m
                _ ->
                  mapM_ (\(s, n) -> local (n:) $ go myId s) $ zip u [0..]
        
    in do
      putStrLn "Dumping macros..."
      encodeFile (outfile ++ ".macros") $ M.toList macros
      putStrLn "Done."
      withFile (outfile ++ ".rules") WriteMode $ \hRules -> do
        putStrLn "Dumping rules..."
        index <- (`execStateT` ruleset0) $ mapM (dumpRule hRules) $ zip [0..] rules
        encodeFile (outfile ++ ".index") $ index & ruleset %~ reverse

-- withRuleset :: FilePath -> (Ruleset Handle -> IO a) -> IO a
-- withRuleset filePath f = do
--   ruleset <- (decodeFile $ filePath ++ ".idx") :: IO (Ruleset ())
--   withFile (filePath ++ ".rules") ReadMode (\handle -> f $ ruleset{_ruleset=handle})
     
(=*=) :: LinkID
      -> LinkID
      -> Bool
(LinkID x dx) =*= (LinkID y dy) = dx == dy && f x y
    where
     f [] _                   = True
     f _  []                  = True
     f (a:t₁) (b:t₂)
         | a == b             = f t₁ t₂
         | any (=='*') [a, b] = True

(*<) :: Char
     -> Char
     -> Ordering
_ *< '*' = EQ
'*' *< _ = EQ
a *< b  = compare a b

flipLink :: LinkID
         -> LinkID
flipLink l@(LinkID _ Plus) = l{_linkDirection = Minus}
flipLink l@(LinkID _ Minus) = l{_linkDirection = Plus}

sortOut :: [Rule] -> (Macros, [Rule'])
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

deMacrify :: Macros
          -> Rule'
          -> Rule'
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
