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
import           Data.Graph.Inductive              (DynGraph, match)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.List                         as List

type Maze = Gr Cell ()

data Cell = Cell (Int, Int) deriving Eq

instance Show Cell where show (Cell p) = show p

-- | Generate a graph representing an n × m grid. The nodes are
-- labelled with their (x, y) position.
grid :: Int -> Int -> Maze
grid width height = Graph.mkGraph nodes edges
  where nodes = zip [0..] $ cell <$> [0..width - 1] <*> [0..height - 1]
        cell x y = Cell (x, y)
        edges = [(n, n', ()) | (n,_) <- nodes, (n',_) <- nodes, n - n' `elem` [1, width]]

dfs [] _                           = []
dfs _ g | Graph.isEmpty g          = []
dfs (v:vs) (match v -> (Just c, g)) = v : dfs next g
  where next = Graph.suc' c ++ vs
dfs (_:vs) g                       = dfs vs g

-- idea: replace list of nodes to visit with graph!

type MazeGen a = forall g. RandomGen g => StateT Maze (Rand g) a

maze width height = go [] (grid width height)
  where go [] _                           = return []
        go _ g | Graph.isEmpty g          = return []
        go (v:vs) (match v -> (Just c, g)) = do
          rest <- go (Graph.suc' c ++ vs) g
          return $ v : rest
        go (_:vs) g                       = go vs g


choose [] = error "Cannot choose from empty list!"
choose ls = do
  i <- getRandomR (0, length ls - 1)
  let x = ls !! i
  return (x, List.delete x ls)

shuffle [] = return []
shuffle ls = do
  (x, xs) <- choose ls
  xs'     <- shuffle xs
  return $ x : xs'
