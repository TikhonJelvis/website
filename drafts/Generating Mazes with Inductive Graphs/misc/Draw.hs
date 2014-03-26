{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Draw where

import           Data.Function         (on)
import qualified Data.List             as List
import           Data.Ord              (comparing)

import           Diagrams.Prelude
import           Diagrams.Backend.SVG

import           Maze

-- type Shape = forall b. Renderable (Path R2) b => Diagram b R2

type Shape = Diagram SVG R2

rectMaze :: Maze -> Shape
rectMaze maze = vcat $ map hcat shapes
  where shapes = map toCell <$> (sortCells (cells maze))

cell :: Shape
cell = rect 10 10

outWall :: Direction -> Shape
outWall Horizontal = rect 10 1 # fc white # translate (r2 (0, 5))
outWall Vertical   = rect 1 10 # fc white # translate (r2 (-5, 0))

inWall :: Direction -> Shape
inWall Horizontal = rect 10 1 # fc white # translate (r2 (0, -5))
inWall Vertical   = rect 1 10 # fc white # translate (r2 (5, 0))

toCell :: Cell -> Shape
toCell (pos, out, inn) = mconcat (outWall <$> out)
                      <> mconcat (inWall <$> inn)
                      <> cell <> text (show pos)

sortCells :: [Cell] -> [[Cell]]
sortCells = map (List.sortBy x) . List.groupBy eq . List.sortBy y
  where x = comparing $ \ ((x, _), _, _) -> x
        y = comparing $ \ ((_, y), _, _) -> y
        eq = (==) `on` \ ((_, y), _, _) -> y
