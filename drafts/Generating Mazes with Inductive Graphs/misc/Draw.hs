{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Draw where

import           Data.Function         (on)
import qualified Data.List             as List
import           Data.Ord              (comparing)

import           Diagrams.Prelude
import           Diagrams.Backend.SVG

import           Maze

type Shape = forall b. Renderable (Path R2) b => Diagram b R2

rectMaze :: Maze -> Shape
rectMaze maze = vcat $ map hcat shapes
  where shapes = map toCell <$> (sortCells (cells maze))

cell :: Shape
cell = rect 10 10

wall :: Direction -> Shape
wall Horizontal = rect 10 2 # fc white # translate (r2 (0, 4))
wall Vertical   = rect 2 10 # fc white # translate (r2 (-4, 0))

toCell :: Cell -> Shape
toCell (pos, out) = mconcat (wall <$> out) <> cell

sortCells :: [Cell] -> [[Cell]]
sortCells = map (List.sortBy x) . List.groupBy eq . List.sortBy y
  where x = comparing $ \ ((x, _), _) -> x
        y = comparing $ \ ((_, y), _) -> y
        eq = (==) `on` \ ((_, y), _) -> y
