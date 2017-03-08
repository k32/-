{-# LANGUAGE FlexibleContexts, TupleSections, TemplateHaskell, DeriveGeneric, LambdaCase #-}
module Voretion.Kobenation (
    trySort
  , natalyze
  , toBase64
  , fromBase64
  , dbgKob
  ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import Control.Monad.State hiding (guard)
import LinkGrammar.AST
import LinkGrammar.Process
import Voretion.Config
import Data.List
import Data.Graph
import Debug.Trace
import Data.PrettyPrint
import Data.Foldable
import Data.Tree.Zipper hiding (last)
import Data.Nontransitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Map as M
import qualified Data.Set as S
import Voretion.Config
import Data.Maybe (mapMaybe, fromJust, maybeToList)
import Data.Either (lefts)
import Data.Function (on)
import Text.Printf
import Data.Tree
import qualified Data.Set as S

-- Debug
import qualified Data.Binary as Bin
import GHC.Generics (Generic)
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString (unpack)
import Data.String (fromString)
import Control.Monad.Trans.Cont (callCC, evalCont)
import Control.Monad.State (get, modify, evalStateT)
import Control.Monad.Except
import Control.Monad.ST
import Data.STRef


{-
* Overview of the Topological Voretion Method:

Kobenation is:

    +-- Vector of NLP Words and "pointers"
    |
    |          +-- Star emphasizes position     +-- What original (simplified)
    |          |   of the current NLP word      |   rule looks like
    v          v                                v
0: |W₁| => [2, *0, 1]  \                     %  W₁ : A- & B+
1: |W₂| => [0, *1]      > Resolved rules     %  W₂ : B-
2: |W₃| => [3, *2, 0]  /                     %  W₃ : C- & A+
3: [C-]       <--- Remaining rules aren't resolved yet
....

An elements of the vector is called called Kob

Now we can translate Kobenation to a set of 1st order constraints:
0: [2:<:0, 0:<:1]
1: [0:<:1]
2: [3:<:2, 2:<:0]
...

2nd order constraints are needed to prevent crossing links like on the
below picture:

%      _____
%     /____ \
%   _/____ \ \
% /  |    \| |
% 3  0     1 2

0: [*0, 1, 2]      % 0 : A+ & B+;
1: [3, 0, *1]      % 1 : C- & A-;
2: [0, *2]         % 2 : B-;
3: [3, 1]          % 3 : C+;

1st order ineqs alone are not sufficient to prevent link intersection:
[[0:<:1, 1:<:2], [3:<:0, 0:<:1], [0:<:2], [3:<:1]] are sortable:
[3, 0, 1, 2]

2nd order constraints are found according to the following algorithm:
let A₀ is an element of a kobenation K. For all pointers in A₀ except
the starred one, lookup the row in K, say A_n, take the first and the
last elements from A_n, then make sure last(A_n) :<: first(A_{n+1})

And simply concatenate all 1st and 2nd order constraints:
[2:<:0, 0:<:1, 0:<:1, 3:<:2, 2:<:0, ...]


Backtrack if the system isn't feasible, otherwise resolve remaining
rows.

Stop if there are no unresolved rules.

-}

type Pointer = Int

type RuleZipper = TreePos Full NodeType

data Kob =
  Resolved {
    _word :: NLPWord
  , _order :: [Pointer]
  } |
  Unresolved Pointer LinkID
  deriving (Show, Generic)
makeLenses ''Kob
instance Bin.Binary Kob

type Kobenation = V.Vector Kob

data DrawAction = Line Int Int | SymbolAt Int Char | Vertical Int deriving Show
               
asciiDraw :: Int -> [DrawAction] -> String
asciiDraw l = V.toList . foldl draw (V.replicate l ' ')
  where
    draw v (Line a b) = v V.//(zip [a'+1..b'-1]['-','-'..]++[(a'+1,','),(b'-1,'.')])
      where a' = min a b
            b' = max a b
    draw v (SymbolAt i s) = v V.//[(i,s)]
    draw v (Vertical i) = case v V.! i of{a|(a==)`any`" |"->v V.//[(i,'|')];_ -> v V.//[(i,'+')]}
                                   
drawKob :: Kobenation -> [Pointer] -> String
drawKob kob order =
  let
    zz f x l = fst $ unzip $ scanl f x l
    sorted = map (\i -> case kob V.! i of
                          (Unresolved ptr _) -> printf "?[%d]" i
                          (Resolved{_word = w}) -> printf "%s[%d]" (_nlpword w) i) order
    positions = drop 1 $ zz wordPos (0,0) sorted
    maxpos = Data.List.last positions + 1
    wordPos (_,l) s = (l+1+t`div`2, l+1+t)
      where t = length (s :: String)
    links = zip [0..] (V.toList kob) >>= getLinks & mapped . both %~ getIdx
      where getIdx = fromJust . (`elemIndex` order)            
    getLinks (i, Resolved{_order = k}) = map (i,) $ filter (>i) k
    getLinks (i, _) = []
    drawLink (_,l) x = (cl, l')
      where x' = map (\(a,b)->(positions!!a-1,positions!!b-1)) x
            l' = l++(x'>>=(\(a,b)->[Vertical a, Vertical b]))
            cl = asciiDraw maxpos (map(uncurry Line)x'++l) ++ '\n':asciiDraw maxpos l'
    bottom = unwords sorted

    splitIntersect [] = []
    splitIntersect l = g:splitIntersect t'
      where (g,t') = go l []
            intersect (a, b) (c, d) | a < c && b < c = False
                                    | a > c && a > d = False
                                    | True = True
            go (a:t) acc | (intersect a) `any` acc = (acc, a:t)
                         | True = go t (a:acc)
            go [] acc = (acc, [])
    upperLayers = reverse $ zz drawLink ([], []) $ splitIntersect $
                    sortBy (compare `on` negate . abs . uncurry (-)) links
  in
    unlines $ reverse $ bottom : upperLayers

data MyState =
  State {
    _currentId
  , _lastResolvedId :: Int
  }

type QuasiLink = Either LinkID Pointer

type Disjunct = ([QuasiLink], [QuasiLink])

type DisjunctZ = (([QuasiLink], [QuasiLink], Maybe QuasiLink)
                 ,([QuasiLink], [QuasiLink], Maybe QuasiLink))

-- Add a + link left of the current one
(+|) :: DisjunctZ -> QuasiLink -> DisjunctZ
d +| l = d&_2._1%~(l:)

-- Add a + link right of the current one
(|+) :: DisjunctZ -> QuasiLink -> DisjunctZ
d |+ l = d&_2._2%~(l:)

-- Add a - link left of the current one
(-|) :: DisjunctZ -> QuasiLink -> DisjunctZ
d -| l = d&_1._1%~(l:)

-- Add a - link right of the current one
(|-) :: DisjunctZ -> QuasiLink -> DisjunctZ
d |- l = d&_1._2%~(l:)

toDisjunct :: DisjunctZ -> Disjunct
toDisjunct ((a,b,x), (c,d,y)) = (reverse a ++ toList x ++ b, reverse c ++ toList y ++ d)

{- | Given a rule, return list of zippers pointing to links mating with
the given one -}
findConnections :: LinkID -- ^ LinkID to match with
                -> Link   -- ^ Rule
                -> [RuleZipper]
findConnections x = go . fromTree
  where go z =
          case label z of
            Link {_link=l}
              | x' =*= l -> z : (go' $ next z)
              | True     -> go' $ next z
            _ ->
              go' (firstChild z) ++ go' (next z)

        go' x = (toList x) >>= go

        x' = flipLink x

dbgKob :: Kobenation
       -> IO ()
dbgKob kob = case ktopSort $ kobConn0 kob of
               (Left _) -> putStrLn $ dbgKob' kob
               (Right ord) -> putStrLn (drawKob kob ord ++ dbgKob' kob)

dbgKob' :: Kobenation -> String
dbgKob' kob = (printf "//Raw: %s\n" $ toBase64 kob) ++ (unlines . map (uncurry f) . zip [0..] $ V.toList kob)
    where f :: Int -> Kob -> String
          f n Resolved{_word=NLPWord{_nlpword=w}, _order=o} = printf "//KOBENATION %d: %s %s" n w (show o)
          f n (Unresolved link p) = printf "//KOBENATION %d: [%d] %s" n link (show p)

natalyze :: (MonadVoretion m)
         => Config
         -> (LinkID -> [Rule'])
         -> V.Vector Rule'
         -> m [NLPWord]
natalyze cfg mate allRules =
  let
    dbg kob | _debug cfg = trace ("let(Right kob)=fromBase64 \"" ++ toBase64 kob ++"\"")
            | True = id

    state₀ = State { 
               _currentId = 0
             , _lastResolvedId = 0
             }

    go :: (MonadVoretion m, MonadState MyState m)
       => Kobenation
       -> m (Kobenation, [Pointer])
    go kob = do
      State{_lastResolvedId=lastId, _currentId=currId} <- get
      case dbg kob $ ktopSort $ kobConn0 kob of
        Left e | _debug cfg -> trace e $ Control.Monad.Voretion.guard False >> error ""
               | otherwise -> Control.Monad.Voretion.guard False >> error ""
        Right sorted ->
          if currId - 1 == lastId
          then return (kob, sorted)
          else go =<< tvoretion cfg mate kob

  in (`evalStateT` state₀) $ do
    Rule' {_lval'=words, _links'=seed} <- pickRandom True allRules
    word <- pickRandom False words
    myId <- nextId
    kob₀ <- addRows (V.singleton undefined) (myId, word) =<< downhill cfg seed
    (kob', order) <- go kob₀
    guardCry (_debug cfg) "FAIL: Empty order" $ not $ null order
    return $ dbg kob' $ map (_word . (kob' V.!)) order

addRows :: (MonadState MyState m)
        => Kobenation
        -> (Pointer, NLPWord)
        -> Disjunct
        -> m Kobenation
addRows k₀ (up, w) (before, after) =
  let
    go (Left _) = nextId
    go (Right x) = return x
  in do {- TODO: Check +/- order accordingly -}
    before' <- mapM go before
    after' <- mapM go after
    let row = before' ++ (up : after')
        new = V.fromList $ map (Unresolved up) $ lefts $ before ++ after
    state <- get
    put $ state{_lastResolvedId=up}
    return $ (k₀ V.// [(up, Resolved w row)]) V.++ new
  
tvoretion :: (MonadState MyState m, MonadVoretion m)
          => Config
          -> (LinkID -> [Rule'])
          -> Kobenation
          -> m Kobenation
tvoretion cfg mate kob₀ = do
  i <- (+1) <$> _lastResolvedId <$> get
  let Unresolved myId link = kob₀ V.! i
  r <- pickRandom True $ mate link
  w <- pickRandom False $ _lval' r
  let conns = findConnections link $ _links' r
      d₀ = case _linkDirection link of
             Minus -> ([], [Right myId])
             Plus  -> ([Right myId], [])
  row <- kobenate cfg d₀ =<< pickRandom True conns
  addRows kob₀ (i, w) row

(\++/) :: Disjunct -> Disjunct -> Disjunct
(a1, b1) \++/ (a2, b2) = (a1 ++ a2, b1 ++ b2)  

kobenate :: (MonadVoretion m)
         => Config
         -> Disjunct
         -> RuleZipper
         -> m Disjunct
kobenate cfg d₀ z = uphill d₀ z 
  where
    climb t x =
      case parent x of
        Just x' -> uphill t x'
        Nothing -> return t

    uphill t x = (if _debug cfg
                    then trace ("UPHILL: (" ++ show t ++ ") "  ++ show (label x))
                    else id) $
      case label <$> parent x of
        Just (LinkAnd{}) -> do
          i <- downhill' cfg $ reverse $ before x
          j <- downhill' cfg $ after x
          climb (i \++/ t \++/ j) x
        _ ->
          case label x of
            MultiConnector{} -> do
              m <- downhill cfg (tree x)
              let t' = t \++/ m
              climb t' x
            _ ->
              climb t x

nextId :: (MonadState MyState m)
       => m Int
nextId = do
  s@State{_currentId=i} <- get
  put $ s{_currentId=i+1}
  return i

downhill' :: (MonadVoretion m)
          => Config
          -> [Link]
          -> m Disjunct
downhill' cfg x = foldl (\++/) ([], []) <$> mapM (downhill cfg) x

downhill :: (MonadVoretion m)
         => Config
         -> Link
         -> m Disjunct
downhill cfg l0@(Node label subforest) =
  case label of
    Optional _ ->
      ifR (_decay_optional cfg)
         {-then-} (downhill cfg $ head subforest)
         {-else-} (return ([], []))
    MultiConnector _ ->
      ifR (_decay_multi cfg)
         {-then-} (do
           a <- downhill cfg l0
           b <- downhill cfg $ head subforest
           return $ a \++/ b)
         {-else-} (return ([], []))
    LinkOr{} ->
      downhill cfg =<< pickRandom True subforest
    LinkAnd{} ->
      downhill' cfg subforest
    Cost{} ->
      downhill' cfg subforest
    EmptyLink ->
      return ([], [])
    Link{_link=i} -> do
      case _linkDirection i of
        Plus  -> return ([], [Left i])
        Minus -> return ([Left i], [])

-- kob2graph :: Kobenation
--           -> Graph
-- kob2graph kob = fst $ graphFromEdges' $ V.ifoldl f [] kob
--   where f l i Resolved{_order=o} = (True,i,up):l
--           where (_,_:up) = break (==i) o
--         f l i Unresolved{} = (False,i,[j]):l
--           where Just j = V.findIndex (\case Resolved{_order=o} -> any (==i) o
--                                             Unresolved{} -> False) kob

fromBase64 :: String -> Either String Kobenation
fromBase64 str = (V.fromList . Bin.decode . fromStrict) <$> (Base64.decode $ fromString str)

toBase64 :: Kobenation -> String
toBase64 = map (toEnum . fromIntegral) . unpack . Base64.encode . toStrict . Bin.encode . V.toList

kobConn0 :: Kobenation
         -> V.Vector ([Int], [Int])
kobConn0 kob = kob'
  where kob' = V.imap conn kob
        
        conn i Resolved {_order=o} = second reverse $ break (==i) o
        conn i Unresolved{} = evalCont $ callCC $ \ret -> V.imapM (f ret) kob'
                                                          >> error "Should not happen!"
          where ok = elem i
                f ret j (d,u) | ok d = ret ([],[j,i])
                              | ok u = ret ([j],[i])
                              | True = return ()

printKobConn k = unlines $ map (\(i,k) -> show i  ++ ": " ++  show k) $ zip [0..] $ V.toList k

ktopSort :: V.Vector ([Int], [Int])
        -> Either String [Int]
ktopSort kob =
  let
    sup u k = do
      let max i = (i,) . head . snd <$> k `MV.read` i
      l <- mapM max u
      kv <- V.freeze k
      case filter (uncurry (==)) l of -- TODO match against the list of downlinks?
        [(a,_)] -> return (Right a)
        x -> return $ Left $ "Topology error: " ++ show x ++ "\nFail\n" ++ printKobConn kv ++ "\nOrig:\n" ++ printKobConn kob

    rewrite k s (i, j) = do
      -- trace (printf "Rewrite for %d (%d -> %d)" i s j) $ return ()
      let f x | x == s && j < 0 = []
              | x == s = [j]
              | otherwise = [x]
      when (j>=0) $ MV.modify k (_1 %~ (i:)) j
      MV.modify k (_2 %~ (>>= f)) i

    go u k =
      readSTRef u >>= \case
        [a] -> return $ Right [a]
        u_ -> sup u_ k >>= \case
          Right s -> do
            modifySTRef' u $ Data.List.delete s
            s_d <- fst <$> MV.read k s
            mapM_ (rewrite k s) $ zip s_d (tail s_d ++ [-1])
            tail <- go u k
            return $ (s:) <$> tail
          Left err ->
            return  $ Left err

  in runST $ do
    u <- newSTRef $ [0..V.length kob-1]
    k <- V.thaw kob
    liftM reverse <$> go u k
