{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-- | A few versions of depth-first search for inductive graphs. A much
--   more comprehensive implementation comes with fgl directly as
--   @Data.Graph.Inductive.Query.DFS@.
-- 
--   This code was written for my blog post on generating mazes:
--   <http://jelv.is/blog/Generating Mazes with Inductive Graphs/>
module DFS where

import           Control.Monad        (liftM)
import           Control.Monad.Random (getRandomR, MonadRandom)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr, Node, (&), match, matchAny)

-- | An unordered map that relabels all the nodes in the graph.
mapNodes :: (n -> n') -> Gr n e -> Gr n' e
mapNodes _ g | Graph.isEmpty g = Graph.empty
mapNodes f (matchAny -> ((in', node, label, out), g)) =
  (in', node, f label, out) & mapNodes f g

-- | The simplest depth-first search which returns a list of the nodes
--   visited, in order.
dfs :: Gr n e -> [Node]
dfs (matchAny -> ((_, start, _, _), graph)) = go [start] graph
  where go [] _                           = []
        go _ g | Graph.isEmpty g          = []
        go (n:ns) (match n -> (Just c, g)) = n : go (Graph.suc' c ++ ns) g
        go (_:ns) g                       = go ns g

-- | A modified version of dfs that returns a list of edges followed
--   rather than just nodes.
edfs :: Gr n e -> [(Node, Node)]
edfs (matchAny -> ((_, start, _, _), graph)) =
  drop 1 $ go [(start, start)] graph
  where go [] _                                = []
        go _ g | Graph.isEmpty g               = []
        go ((p, n):ns) (match n -> (Just c, g)) =
          (p, n) : go (map (n,) (Graph.suc' c) ++ ns) g
        go (_:ns) g                            = go ns g

-- | A version of edfs where the order neighbors are visited is
--   random.
edfsR :: MonadRandom m => Gr n e -> m [(Node, Node)]
edfsR (matchAny -> ((_, start, _, _), graph)) =
  liftM (drop 1) $ go [(start, start)] graph
  where go [] _                                = return []
        go _ g | Graph.isEmpty g               = return []
        go ((p, n):ns) (match n -> (Just c, g)) = do
          edges <- shuffle $ map (n,) (Graph.suc' c)
          liftM ((p, n) :) $ go (edges ++ ns) g
        go (_:ns) g                            = go ns g

-- | A naïve but unbiased list shuffle. Warning: it runs in O(n²)
--   time!
shuffle :: MonadRandom m => [a] -> m [a]
shuffle [] = return []
shuffle ls = do
  (x, xs) <- choose ls
  liftM (x :) $ shuffle xs
  where choose [] = error "Cannot choose from emtpy list!"
        choose ls = do
          i <- getRandomR (0, length ls - 1)
          let (as, x:bs) = splitAt i ls
          return (x, as ++ bs)
