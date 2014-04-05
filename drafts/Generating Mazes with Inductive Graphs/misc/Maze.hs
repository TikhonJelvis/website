import DFS

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr)

data Orientation = Horizontal | Vertical deriving (Show, Eq)

data Wall = Wall (Int, Int) Orientation deriving (Show, Eq)

type Maze = Gr () Wall

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
        wall n = let (y, x) = n `divMod` width in (x, y)
