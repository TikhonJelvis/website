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
import           Data.Graph.Inductive (Gr, (&), match, matchAny)

-- | Return an arbitrary node from the graph. Fails on empty graphs.
ghead :: Gr a b -> Graph.Node
ghead graph | Graph.isEmpty graph           = error "Empty graph!"
ghead (matchAny -> ((_, node, _, _), graph)) = node

-- | An unordered map that relabels all the nodes in the graph.
mapNodes :: (n -> n') -> Gr n e -> Gr n' e
mapNodes _ g | Graph.isEmpty g = Graph.empty
mapNodes f (matchAny -> ((in', node, label, out), g)) =
  (in', node, f label, out) & mapNodes f g

-- | The simplest depth-first search which returns a list of the nodes
--   visited, in order. 
dfs :: Graph.Node -> Gr n e -> [Graph.Node]
dfs start graph = go [start] graph
  where go [] _                           = []
        go _ g | Graph.isEmpty g          = []
        go (n:ns) (match n -> (Just c, g)) =
          n : go (Graph.neighbors' c ++ ns) g
        go (_:ns) g                       = go ns g

-- | A modified version of dfs that returns a list of edges followed
--   rather than just nodes.
edfs :: Graph.Node -> Gr n e -> [Graph.Edge]
edfs start graph = drop 1 $ go [(start, start)] graph
  where go [] _                                = []
        go _ g | Graph.isEmpty g               = []
        go ((p, n):ns) (match n -> (Just c, g)) =
          (p, n) : go (map (n,) (Graph.neighbors' c) ++ ns) g
        go (_:ns) g                            = go ns g

-- | A version of edfs where the order neighbors are visited is
--   random.
edfsR :: MonadRandom m => Graph.Node -> Gr n e -> m [Graph.Edge]
edfsR start graph = liftM (drop 1) $ go [(start, start)] graph
  where go [] _                                = return []
        go _ g | Graph.isEmpty g               = return []
        go ((p, n):ns) (match n -> (Just c, g)) = do
          edges <- shuffle $ map (n,) (Graph.neighbors' c)
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
