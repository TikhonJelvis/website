{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module DFS where

import Control.Monad
import Control.Monad.Random

import Data.Graph.Inductive
import Data.List

mapNodes :: (n -> n') -> Gr n e -> Gr n' e
mapNodes f g | isEmpty g = empty
mapNodes f (matchAny -> ((in', node, label, out), g)) =
  (in', node, f label, out) & mapNodes f g

dfs :: Gr n e -> [Node]
dfs (matchAny -> ((_, start, _, _), graph)) = go [start] graph
  where go [] _                           = []
        go _ g | isEmpty g                = []
        go (n:ns) (match n -> (Just c, g)) = n : go (suc' c ++ ns) g 
        go (_:ns) g                       = go ns g

edfs :: Gr n e -> [(Node, Node)]
edfs (matchAny -> ((_, start, _, _), graph)) = drop 1 $ go [(start, start)] graph
  where go [] _                                = []
        go _ g | isEmpty g                     = []
        go ((p, n):ns) (match n -> (Just c, g)) =
          (p, n) : go (map (n,) (suc' c) ++ ns) g
        go (_:ns) g                            = go ns g

edfsR :: MonadRandom m => Gr n e -> m [(Node, Node)]
edfsR (matchAny -> ((_, start, _, _), graph)) = liftM (drop 1) $ go [(start, start)] graph
  where go [] _                                = return []
        go _ g | isEmpty g                     = return []
        go ((p, n):ns) (match n -> (Just c, g)) = do
          edges <- shuffle $ map (n,) (suc' c)
          liftM ((p, n) :) $ go (edges ++ ns) g
        go (_:ns) g                            = go ns g

shuffle :: MonadRandom m => [a] -> m [a]
shuffle [] = return []
shuffle ls = do
  (x, xs) <- choose ls
  liftM (x :) $ shuffle xs
  where choose [] = error "Cannot choose from emtpy list!"
        choose ls = do
          i <- getRandomR (0, length ls - 1)
          let (as, x:bs) = splitAt i ls
          return (x, as ++ bs)
