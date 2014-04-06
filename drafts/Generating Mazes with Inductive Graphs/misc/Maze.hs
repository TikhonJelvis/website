module Maze where

import           DFS

import           Control.Monad        (liftM)
import           Control.Monad.Random (MonadRandom)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr)
import           Data.List            ((\\))

data Orientation = Horizontal | Vertical deriving (Show, Eq)

data Wall = Wall (Int, Int) Orientation deriving (Eq)

instance Show Wall where
  show (Wall (x, y) Horizontal) = show (x, y) ++ " —"
  show (Wall (x, y) Vertical)   = show (x, y) ++ " |"

type Grid = Gr () Wall

grid :: Int -> Int -> Grid
grid width height = Graph.mkGraph nodes edges
  where nodes = [(node, ()) | node <- [0..width * height - 1]]
        edges = [(n, n', wall n Vertical) |
                 (n, _) <- nodes,
                 (n', _) <- nodes,
                 n - n' == 1 && n `mod` width /= 0 ]
             ++ [(n, n', wall n Horizontal) |
                 (n,_) <- nodes,
                 (n',_) <- nodes,
                 n - n' == width ]
        wall n = let (y, x) = n `divMod` width in Wall (x, y)

maze :: MonadRandom m => Int -> Int -> m [Graph.Edge]
maze width height = liftM (Graph.edges graph \\) $ edfsR graph
  where graph = grid width height                    
