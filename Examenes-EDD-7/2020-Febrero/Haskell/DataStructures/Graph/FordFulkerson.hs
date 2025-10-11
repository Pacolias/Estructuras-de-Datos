-------------------------------------------------------------------------------
-- Ford-Fulkerson Algorithm. Maximal flow for a weighted directed graph.
--
-- Student's name: Paco Molina Cuenca
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática D
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.FordFulkerson where

import Data.List  ((\\))
import DataStructures.Graph.WeightedDiGraph
import DataStructures.Graph.WeightedDiGraphBFT

maxFlowPath :: Path (WDiEdge a Integer) -> Integer
maxFlowPath path = minimum [w | E _ w _ <- path]

updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge x y p edges
    | not (null [w | E u w v <- edges, u == x, v == y]) = [E x (w+p) y | E u w v <- edges, u == x, v == y, w+p /= 0] ++ [E u w v | E u w v <- edges, not(u == x && v == y)]
    | otherwise                                         = E x p y : edges

updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges path p edges = foldr (\(E x _ y) -> updateEdge x y p) edges path

addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow x y p sol
    | not (null [w | E u w v <- sol, u == x, v == y])         = [E x (w+p) y | E u w v <- sol, u == x, v == y] ++ [E u w v | E u w v <- sol, not(u == x && v == y)]
    | not (null [w | E u w v <- sol, u == y, v == x, w == p]) = [E u w v | E u w v <- sol, not(u == y && v == x && w == p)]
    | not (null [w | E u w v <- sol, u == y, v == x, w < p])  = [E x (p-w) y | E u w v <- sol, u == y, v == x, w < p] ++ [E u w v | E u w v <- sol, not(u == y && v == x)]
    | not (null [w | E u w v <- sol, u == y, v == x, w > p])  = [E y (w-p) x | E u w v <- sol, u == y, v == x, w > p] ++ [E u w v | E u w v <- sol, not(u == y && v == x)]
    | otherwise                                                 = E x p y : sol


addFlows :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlows path p sol = foldr (\(E x _ y) -> addFlow x y p) sol path

fordFulkerson :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer]
fordFulkerson g src dst = fordFulkersonRec [] g 
    where 
        fordFulkersonRec sol wdg = 
            case bftPathTo wdg src dst of 
                Nothing     -> sol 
                Just path   -> fordFulkersonRec sol' wdg' 
                    where 
                        mf      = maxFlowPath path 
                        edges   = weightedDiEdges wdg 
                        edges'  = updateEdges path (-mf) edges 
                        edges'' = updateEdges (map (\(E x p y) -> E y p x) path) mf edges'
                        wdg'    = mkWeightedDiGraphEdges (vertices wdg) edges'' 
                        sol'    = addFlows path mf sol

maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow path src = sum [w | E u w _ <- path, u == src]

maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst set = sum [w | E u w v <- weightedDiEdges g, u `elem` k, v `notElem` k] - sum [w | E u w v <- weightedDiEdges g, u `notElem` k, v `elem` k]
    where 
        k = if src `elem` set then set else vertices g \\ set

-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial 
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium g src dst = and [inDegree g v == outDegree g v | v <- vertices g]

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks g = ([v | v <- vertices g, inDegree g v == 0], [v | v <- vertices g, outDegree g v == 0])

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink g newSrc newDst 
    | length sources > 1 && length sinks > 1 = mkWeightedDiGraphEdges (newSrc : newDst : vertices g) ([E newSrc (pesoSrc v) v | v <- sources] ++ [E v (pesoDst v) newDst | v <- sources] ++ weightedDiEdges g)
    | length sources > 1                     = mkWeightedDiGraphEdges (newSrc : vertices g) ([E newSrc (pesoSrc v) v | v <- sources] ++ weightedDiEdges g) 
    | length sinks > 1                       = mkWeightedDiGraphEdges (newDst : vertices g) ([E v (pesoDst v) newDst | v <- sources] ++ weightedDiEdges g)
    | otherwise                              = g
    where 
        (sources, sinks) = sourcesAndSinks g

        pesoSrc v = sum [p | E x p _ <- weightedDiEdges g, x == v]
        pesoDst v = sum [p | E _ p y <- weightedDiEdges g, y == v]