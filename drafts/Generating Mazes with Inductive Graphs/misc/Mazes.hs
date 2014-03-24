{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}
module Mazes where

import           Control.Applicative               ((<$>), (<*>))
import qualified Control.Monad.Random              as Random
import           Control.Monad.Random              (MonadRandom(..), RandomGen, Rand)
import           Control.Monad.State

import qualified Data.Graph.Inductive              as Graph
import           Data.Graph.Inductive              (DynGraph, Graph, match)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.List                         as List

type Maze = Gr Cell ()

type Cell = (Int, Int)

-- | Generate a graph representing an n × m grid. The nodes are
-- labelled with their (x, y) position.
grid :: Int -> Int -> Maze
grid width height = Graph.mkGraph nodes edges
  where nodes = zip [0..] $ (,) <$> [0..width - 1] <*> [0..height - 1]
        edges = [(n, n', ()) | (n,_) <- nodes, (n',_) <- nodes, n - n' `elem` [1, width]]

-- | Generate a depth-first traversal of the given graph.
dfs :: Graph g => [Graph.Node] -> g a b -> [Graph.Node]
dfs [] _                           = []
dfs _ g | Graph.isEmpty g          = []
dfs (v:vs) (match v -> (Just c, g)) = v : dfs next g
  where next = Graph.suc' c ++ vs
dfs (_:vs) g                       = dfs vs g

-- | Generate a maze by traversing a graph and deleting any edges we
--   follow. The remaining edges then form the "walls" of the maze.
maze :: MonadRandom m => Int -> Int -> m Maze
maze width height = execStateT (go (Graph.nodes cells) cells) cells
  where cells = grid width height

        go ls g | null ls || Graph.isEmpty g = return []
        go (v:vs) (match v -> (Just c, g))   = do
          (n:ns) <- shuffle $ Graph.suc' c
          modify $ Graph.delEdge (v, n) . Graph.delEdge (n, v)
          rest   <- go (n : ns ++ vs) g
          return $ v : rest
        go (_:vs) g                         = go vs g

-- | Choose a random element out of a list.
choose :: (MonadRandom m) => [a] -> m (a, [a])
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
  xs'     <- shuffle xs
  return $ x : xs'
