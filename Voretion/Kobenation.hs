{-# LANGUAGE FlexibleContexts, TupleSections, TemplateHaskell #-}
module Voretion.Kobenation (
    trySort
  , natalyze
  ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import Control.Monad.State hiding (guard)
import LinkGrammar.AST
import LinkGrammar.Process
import Voretion.Config
import Data.List
import Debug.Trace
import Data.PrettyPrint
import Data.Foldable
import Data.Tree.Zipper hiding (last)
import Data.Nontransitive
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Voretion.Config
import Data.Maybe (mapMaybe, fromJust, maybeToList)
import Data.Either (lefts)
import Data.Function (on)
import Text.Printf
    
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
  deriving (Show)
makeLenses ''Kob

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
    sorted = map (kob V.!) order
    positions = drop 1 $ zz wordPos (0,0) sorted
    maxpos = Data.List.last positions + 1
    wordPos (_,l) (Unresolved ptr _) = (l+2, l+3)
    wordPos (_,l) (Resolved{_word = w}) = (l+1+t`div` 2, l+1+t)
      where t = length $ _nlpword w
    links = zip [0..] (V.toList kob) >>= getLinks & mapped . both %~ getIdx
      where getIdx = fromJust . (`elemIndex` order)            
    getLinks (i, Resolved{_order = k}) = map (i,) $ filter (>i) k
    getLinks (i, _) = []
    drawLink (_,l) x = (cl, l')
      where x' = map (\(a,b)->(positions!!a-1,positions!!b-1)) x
            l' = l++(x'>>=(\(a,b)->[Vertical a, Vertical b]))
            cl = asciiDraw maxpos (map(uncurry Line)x'++l) ++ '\n':asciiDraw maxpos l'
    bottom = unwords $ map f sorted
        where f (Resolved w _) = w ^. nlpword
              f (Unresolved p _) = "??"

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

dbgKob :: Config
       -> Kobenation
       -> Maybe [Pointer]
       -> a
       -> a
dbgKob cfg kob Nothing | _debug cfg =
                             trace (dbgKob' kob ++ "//!!!!!! FAIL: Order failure !!!!!!!!!!!")
                       | True = id
dbgKob cfg kob (Just ord) | _debug cfg = trace (drawKob kob ord ++ dbgKob' kob)
                          | True       = id

dbgKob' :: Kobenation -> String
dbgKob' = unlines . map (uncurry f) . zip [0..] . V.toList
    where f :: Int -> Kob -> String
          f n Resolved{_word=NLPWord{_nlpword=w}, _order=o} = printf "//KOBENATION %d: %s %s" n w (show o)
          f n (Unresolved link p) = printf "//KOBENATION %d: [%d] %s" n link (show p)
                                 
dup f a = f a a

natalyze :: (MonadVoretion m)
         => Config
         -> (LinkID -> [Rule'])
         -> V.Vector Rule'
         -> m [NLPWord]
natalyze cfg mate allRules =
  let
    state₀ = State { 
               _currentId = 0
             , _lastResolvedId = 0
             }

    go :: (MonadVoretion m, MonadState MyState m)
       => Kobenation
       -> m (Kobenation, [Pointer])
    go kob = do
      State{_lastResolvedId=lastId, _currentId=currId} <- get
      sorted <- liftMaybe $ dup (dbgKob cfg kob) $ trySort $ getConstraints cfg kob
      if currId - 1 == lastId
         then return (kob, sorted)
         else do
           go =<< tvoretion cfg mate kob

  in (`evalStateT` state₀) $ do
    Rule' {_lval'=words, _links'=seed} <- pickRandom True allRules
    word <- pickRandom False words
    myId <- nextId
    kob₀ <- addRows (V.singleton undefined) (myId, word) =<< downhill cfg seed
    (kob', order) <- go kob₀
    guardCry (_debug cfg) "FAIL: Empty order" $ not $ null order
    return $ dbgKob cfg kob' (Just order) $ map (_word . (kob' V.!)) order

zeroorder :: Kobenation -> [Ineq Pointer]
zeroorder = nub . concat . V.toList . V.imap (\i k->case k of
                                                Unresolved{} -> []
                                                Resolved{_order=o} -> resolved i o)
    where resolved i = uncurry (++) . (map (:<:i) *** (map (i:<:) . tail)) . break (==i)
           
resolved :: Kob -> Bool
resolved (Resolved _ _) = True
resolved _ = False

order2nd :: Config
         -> Kobenation
         -> Maybe [Ineq Pointer] -- TODO: Ugly O(n^2) algorithm
order2nd cfg kob = do
  let links' = zeroorder kob
  grouped <- tryGroup links'
  if _debug cfg then
      trace ("// 0-order links: "++show links'++"\n// 0-order groups: " ++ show grouped) $
            return ()
  else
      return ()
  let grouped' = V.generate (length $ concat grouped) findIdx
      same = (==) `on` (grouped' V.!)
      less = (<) `on` (grouped' V.!)

      findIdx a = fromJust $ findIndex (elem a) grouped

      pairs [] = []
      pairs (a:t) = map (\i->[a,i]) t ++ pairs t
      
      fffindme args@[(a:<:b),(c:<:d)] | a == c || a == d || b == c || b == d = Nothing
                                      | a `less` c && b `less` d = Just $ dbg $ b:<:c
                                      | c `less` a && d `less` b = Just $ dbg $ d:<:a
                                      -- a == b
                                      -- | a `same` c && b `less` d = Just $ c:<:a
                                      -- | a `same` c && d `less` b = Just $ a:<:c
                                      -- -- b == d
                                      -- | b `same` d && a `less` c = Just $ d:<:b
                                      -- | b `same` d && c `less` a = Just $ b:<:d
                                      | True = Nothing
                                     where dbg x  | _debug cfg =
                                                      trace ("// "++show args++" -> "++show x) x
                                                  | True = x

  return $ nub $ mapMaybe fffindme $ pairs links'
    
getConstraints :: Config
               -> Kobenation
               -> [Ineq Pointer]
getConstraints cfg kob =
  let    
    resRows = map _order $ filter resolved $ V.toList kob

    order1st l = nub $ map (uncurry (:<:)) $ zip l $ drop 1 l

    digraph = "xx[shape=plaintext label=\"" ++ show (tryGroup constr1st) ++ "\"];" ++
              (constr1st >>= (\(a:<:b)->printf "%d -> %d [color=\"red\"];" b a)) ++
              (constr2nd >>= (\(a:<:b)->printf "%d -> %d [color=\"blue\"];" b a)) ++
              (zeroorder kob >>= (\(a:<:b)->printf "%d -> %d [color=\"black\"];" b a))
    -- order1' _ _[_]acc=filter(\(a:<:b)->a/=b)acc
    -- order1' i l(a:r:t)acc
    --     |i==a=order1' i a(r:t)acc
    --     |True=case kob V.!a of
    --             Resolved{_order=o}->order1' i a(r:t) $ l:<:head o:last o:<:r:acc
    --             _->order1' i a(r:t)acc

    -- constr1'=V.imap(\i rr -> case rr of
    --                            Resolved{_order=(a:t)}->order1' i a t[]
    --                            _ -> [])kob&V.foldl1'(++)

    constr1st = nub $ resRows >>= order1st -- ++ constr1'
    constr2nd = case order2nd cfg kob of
                  Just x -> x
                  Nothing -> []

    dbg | _debug cfg =
           trace ("//1st order constraints: " ++ show constr1st ++
                  -- "\n1' order constraints: " ++ show constr1' ++ 
                  "\n//2nd order constraints: " ++ show constr2nd ++
                  "\ndigraph {" ++ digraph ++ "}")
        | True = id
  in
    dbg $ constr1st ++ constr2nd

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
