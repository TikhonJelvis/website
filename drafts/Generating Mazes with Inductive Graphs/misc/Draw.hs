{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}
module Draw where

import           Control.Applicative      ((<$>), (<*>))
import           Control.Monad            (forM_)

import           Data.Function            (on)
import qualified Data.List                as List
import           Data.Ord                 (comparing)

import qualified Data.Graph.Inductive     as Graph
import           Data.Graph.Inductive     (DynGraph, Graph, match, (&))

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.Rendering.Cairo (Render)

import           Maze

data Config = Config { step :: Int
                     , wall :: Int
                     , width :: Int
                     , height :: Int
                     }

defaults :: Config
defaults = Config { step = 10
                  , wall = 2
                  , width = 404
                  , height = 404
                  }

rectangle :: (Integral n) => n -> n -> n -> n -> Render ()
rectangle x y width height =
  Cairo.rectangle (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

renderMaze :: Config -> Maze -> Render ()
renderMaze Config {..} maze = do
  Cairo.setSourceRGB 0.02 0.24 0.54
  Cairo.setLineWidth 5

  rectangle 0 0 width height
  Cairo.stroke

  forM_ (walls maze) $ \ ((x, y), dir) -> case dir of
    Horizontal -> rectangle (x * step) (y * step) (step + wall) wall >> Cairo.fill
    Vertical   -> rectangle (x * step) (y * step) wall (step + wall) >> Cairo.fill

mazePNG :: Config -> FilePath -> Maze -> IO ()
mazePNG config@Config {..} file maze =
  Cairo.withImageSurface Cairo.FormatARGB32 width height $ \ surface -> do
    Cairo.renderWith surface $ renderMaze config maze
    Cairo.surfaceWriteToPNG surface file
