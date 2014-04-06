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

bothN :: Graph.Node -> Gr a b -> Gr a b -> Bool
bothN node g₁ g₂ = node `elem` Graph.nodes g₁ && node `elem` Graph.nodes g₂

bothE :: Graph.Edge -> Gr a b -> Gr a b -> Bool
bothE edge g₁ g₂ = edge `elem` Graph.edges g₁ && edge `elem` Graph.edges g₂

nodeStyle :: Graph.Node -> Graph.Node -> Gr a b -> Gr a b -> String
nodeStyle n node full rest
  | n == node          = [str|[color="$blue$" style="filled" fillcolor="$lightBlue$"]|]
  | bothN n full rest = ""
  | otherwise         = [str|[color="white" style="filled" fillcolor="white" fontcolor="white"]|]

edgeStyle :: Graph.Edge -> Graph.Node -> Gr a b -> Gr a b -> String
edgeStyle (a, b) node full rest
  | a == node || b == node    = [str|[color="$blue$"]|]
  | bothE (a, b) full rest = ""
  | otherwise              = [str|[color="white"]|]

decomposition full (_, node, _, _) rest = [str|
digraph fgl {
	margin = "0"
	page = "4"
	size = "4"
	ratio = "fill"
	#n in Graph.nodes full:$:n$ $nodeStyle n node full rest$|
        # 
        #(a, b) in Graph.edges full:$:a$ -> $:b$ $edgeStyle (a, b) node full rest$|
        #
}
|]

anyViz full (matchAny -> (ctx, graph)) = decomposition full ctx graph

nViz full n (match n -> (Just ctx, graph)) = decomposition full ctx graph

normal graph = [str|
digraph fgl {
	margin = "0"
	page = "4"
	size = "4"
	ratio = "fill"
	#node in Graph.nodes graph:$:node$|
        # 
        #(a, b, l) in Graph.labEdges graph:$:a$ -> $:b$ [label="$:l$"]|
        #
}                
|]
