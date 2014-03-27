{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
module Maze where

import           Control.Applicative               ((<$>), (<*>))
import qualified Control.Monad.Random              as Random
import           Control.Monad.Random              (MonadRandom(..), RandomGen, Rand)
import           Control.Monad.State

import qualified Data.Graph.Inductive              as Graph
import           Data.Graph.Inductive              (DynGraph, Graph, match, (&))
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.List                         as List

import           Debug.Trace                       (trace, traceShow)

data Shape = Square deriving (Show, Eq)

type Maze = Gr Shape (Direction, Bool)

type Pos = (Int, Int)

newtype Direction = Direction Rational deriving (Eq, Num)

instance Show Direction where show (Direction d) = show d

data Wall = Wall Direction Bool deriving (Show, Eq)

-- | Generate a graph representing an n × m grid. The nodes are
-- labelled with their (x, y) position.
grid :: Int -> Int -> Maze
grid width height = Graph.mkGraph nodes edges
  where nodes = zip [0..height * width - 1] $ repeat Square
        edges = [(n, n', (180, True)) | (n,_) <- nodes, (n',_) <- nodes, n - n' == 1 && n `mod` width /= 0]
             ++ [(n, n', (90, True)) | (n,_) <- nodes, (n',_) <- nodes, n - n' == width]

-- | Generate a depth-first traversal of the given graph.
dfs [] _                           = []
dfs _ g | Graph.isEmpty g          = []
dfs (v:vs) (match v -> (Just c, g)) = v : dfs (Graph.suc' c ++ vs) g
dfs (_:vs) g                       = dfs vs g

edfs [] _                                   = []
edfs _ g | Graph.isEmpty g                  = []
edfs ((v, p) : vs) (match v -> (Just c, g)) =
  (v, p) : edfs (map (v,) (Graph.suc' c) ++ vs) g
edfs (_:vs) g                               = edfs vs g

edfsR [] _                                   = return []
edfsR _ g | Graph.isEmpty g                  = return []
edfsR ((v, p) : vs) (match v -> (Just c, g)) =
  do next <- shuffle $ (v,) <$> Graph.suc' c
     ((v, p) :) <$> edfsR (next ++ vs) g
edfsR (_:vs) g                                = edfsR vs g

-- | Generate a maze by traversing a graph and deleting any edges we
--   follow. The remaining edges then form the "walls" of the maze.
maze :: (Functor m, MonadRandom m) => Int -> Int -> m Maze
maze width height = execStateT (go [(0, 0)] cells) cells
  where cells = grid width height
        go [] _                           = return ()
        go ls g | Graph.isEmpty g         = return ()
        go ((v, parent):vs) (match v -> (Just c, g)) = do
          next <- shuffle $ zip (Graph.neighbors' c) (repeat v)
          modify $ toggleEdge v parent
          go (next ++ vs) g
        go (_:vs) g = go vs g
        toggleEdge n p (match n -> (Just (inn, _, l, out), g)) =
          (map (toggle p) inn, n, l, map (toggle p) out) & g
        toggle p ((dir, wall), p') = if p == p' then ((dir, not wall), p') else ((dir, wall), p')

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
