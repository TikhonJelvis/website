{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}
module Maze where

import           Control.Applicative               ((<$>), (<*>))
import qualified Control.Monad.Random              as Random
import           Control.Monad.Random              (MonadRandom(..), RandomGen, Rand)
import           Control.Monad.State

import qualified Data.Graph.Inductive              as Graph
import           Data.Graph.Inductive              (DynGraph, Graph, match)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.List                         as List

import           Debug.Trace                       (trace, traceShow)

type Maze = Gr Pos Direction

type Pos = (Int, Int)

data Direction = Horizontal | Vertical deriving (Show, Eq)

-- | A cell and its edges: first outgoing then incoming.
type Cell = (Pos, [Direction])

-- | Extract the walls from a maze graph.
cells :: Maze -> [Cell]
cells maze = toWalls <$> Graph.nodes maze
  where toWalls node =
          let walls = dir <$> Graph.out maze node in
           case Graph.lab maze node of
            Just cell -> (cell, walls)
            Nothing   -> error "Malformed graph!"
        dir (_, _, d) = d

-- | Generate a graph representing an n × m grid. The nodes are
-- labelled with their (x, y) position.
grid :: Int -> Int -> Maze
grid width height = Graph.mkGraph nodes edges
  where nodes = zip [0..] [(x, y) | (y, x) <- map (`divMod` width) [0..width * height - 1]]
        edges = [(n, n', Vertical) | (n,_) <- nodes, (n',_) <- nodes, n - n' == 1 && n `mod` width /= 0]
             ++ [(n, n', Horizontal) | (n,_) <- nodes, (n',_) <- nodes, n - n' == width]

-- | Generate a depth-first traversal of the given graph.
dfs :: Graph g => [Graph.Node] -> g a b -> [Graph.Node]
dfs [] _                           = []
dfs _ g | Graph.isEmpty g          = []
dfs (v:vs) (match v -> (Just c, g)) = v : dfs (Graph.suc' c ++ vs) g
dfs (_:vs) g                       = dfs vs g

-- | Generate a maze by traversing a graph and deleting any edges we
--   follow. The remaining edges then form the "walls" of the maze.
maze :: (Functor m, MonadRandom m) => Int -> Int -> m Maze
maze width height = execStateT (go [(0, 0)] cells) cells
  where cells = grid width height
        go [] _                           = return ()
        go ls g | Graph.isEmpty g         = return ()
        go ((v, parent):vs) (match v -> (Just c, g)) = do
          next <- shuffle $ zip (Graph.neighbors' c) (repeat v)
          modify $ Graph.delEdges [(v, parent), (parent, v)]
          go (next ++ vs) g
        go (_:vs) g = go vs g

-- | Choose a random element out of a list.
choose [] = error "Cannot choose from empty list!"
choose ls = do
  i <- getRandomR (0, length ls - 1)
  let (as, x:bs) = List.splitAt i ls
  return (x, as ++ bs)

-- | Return a random permutation of a list. O(n²) time—should only be
--   used for small lists!
shuffle [] = return []
shuffle ls = do
  (x, xs) <- choose ls
  (x :) <$> shuffle xs
