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
maxFlowPath path = minimum [w | E x w y <- path]

updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge x y p edges
    | not(null([w | E x' w y' <- edges, x' == x, y' == y])) = [E x (w+p) y | E x' w y' <- edges, x' == x, y' == y, w+p /= 0] ++ [E x' w y' | E x' w y' <- edges, not(x' == x && y' == y)]
    | otherwise                                             = E x p y : edges

updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges path p edges = foldr (\(E x w y) -> updateEdge x y p) edges path

addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow x y p sol 
    | not(null([w | E x' w y' <- sol, x' == x, y' == y]))           = [E x (w+p) y | E x' w y' <- sol, x' == x, y' == y] ++ [E x' w y' | E x' w y' <- sol, not(x' == x && y' == y)]
    | not(null([w | E y' w x' <- sol, y' == y, x' == x, w == p]))   = [E y' w x' | E y' w x' <- sol, not(y' == y && x' == x && w == p)]
    | not(null([w | E y' w x' <- sol, y' == y, x' == x, w < p]))    = [E x (p-w) y | E y' w x' <- sol, y' == y, x' == x, w < p] ++ [E y' w x' | E y' w x' <- sol, not(y' == y && x' == x && w < p)]
    | not(null([w | E y' w x' <- sol, y' == y, x' == x, w > p]))    = [E y (w-p) x | E y' w x' <- sol, y' == y, x' == x, w > p] ++ [E y' w x' | E y' w x' <- sol, not(y' == y && x' == x && w > p)]
    | otherwise                                                     = E x p y : sol

addFlows :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlows path p sol = foldr (\(E x w y) -> addFlow x y p) sol path

fordFulkerson :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer]
fordFulkerson g src dst = fordFulkersonRec g []
    where 
        fordFulkersonRec wdg sol = 
            case bftPathTo wdg src dst of 
                Nothing     -> sol 
                Just path   -> fordFulkersonRec wdg' sol'
                    where 
                        mf      = maxFlowPath path 
                        edges   = weightedDiEdges wdg 
                        edges'  = updateEdges path (-mf) edges
                        edges'' = updateEdges (map (\(E x w y) -> E y w x) path) mf edges'
                        wdg'    = mkWeightedDiGraphEdges (vertices wdg) edges''
                        sol'    = addFlows path mf sol 

maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow sol src = sum [w | E x w y <- sol, x == src]

maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst set = sum [w | E x w y <- weightedDiEdges g, x `elem` set, y `notElem` set] - sum [w | E x w y <- weightedDiEdges g, x `notElem` set, y `elem` set]

-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial 
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium g src dst = foldr (&&) True (map (\v -> outDegree g v == inDegree g v) (vertices g \\ [src, dst]))

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks g = ([v | v <- vertices g, inDegree g v == 0], [v | v <- vertices g, outDegree g v == 0])

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink g newSrc newDst
    | not(null fuentes) && not(null sumideros)  = mkWeightedDiGraphEdges (newSrc : newDst : vertices g) ([E newSrc (sum [w | E f' w y <- weightedDiEdges g, f' == f]) f | f <- fuentes] ++ [E s (sum [w | E x w s' <- weightedDiEdges g, s' == s]) newDst | s <- sumideros] ++ weightedDiEdges g)
    | not(null fuentes)                         = mkWeightedDiGraphEdges (newSrc : vertices g) ([E newSrc (sum [w | E f' w y <- weightedDiEdges g, f' == f]) f | f <- fuentes] ++ weightedDiEdges g)
    | not(null sumideros)                       = mkWeightedDiGraphEdges (newDst : vertices g) ([E s (sum [w | E x w s' <- weightedDiEdges g, s' == s]) newDst | s <- sumideros] ++ weightedDiEdges g)
    | otherwise                                 = g
    where 
        (fuentes, sumideros) = sourcesAndSinks g
