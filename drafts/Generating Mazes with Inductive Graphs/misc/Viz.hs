{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE ViewPatterns    #-}
module Viz where

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr, match, matchAny)

import           Data.String.Interpolation (str)

example :: Gr () ()
example = Graph.mkGraph nodes edges
  where nodes = [(x, ()) | x <- [1..7]]
        edges = [(x, y, ()) | x <- [1..7], y <- [1..7], x /= y, x - y `elem` [3..5]]

blue = "#0b4faa"

lightBlue = "#90A9CB"

graphViz ctx@(_, node, _, _) graph = [str|
digraph fgl {
	margin = "0"
	page = "4"
	size = "4"
	ratio = "fill"
        $:node$ [color="$blue$" style="filled" fillcolor="$lightBlue$"]
	#node in Graph.nodes graph:$:node$|
        # 
        #node' in Graph.suc' ctx:$:node$ -> $:node'$ [color="$blue$"]|
        # 
        #node' in Graph.pre' ctx:$:node'$ -> $:node$ [color="$blue$"]|
        #
        #(a, b) in Graph.edges graph:$:a$ -> $:b$|
        #
}
|]

anyViz (matchAny -> (ctx, graph)) = graphViz ctx graph

nViz n (match n -> (Just ctx, graph)) = graphViz ctx graph
