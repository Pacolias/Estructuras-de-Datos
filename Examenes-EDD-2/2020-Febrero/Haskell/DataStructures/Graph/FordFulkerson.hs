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
maxFlowPath path = minimum (map (\(E s w d) -> w) path)

updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge x y p edges
    | length [E s w d | (E s w d) <- edges, s == x, d == y] == 1 = [E s (p+w) d | (E s w d) <- edges, s == x, d == y, p+w /= 0] ++ [E s w d | (E s w d) <- edges, not(s == x && d == y)]
    | otherwise                                                  = E x p y : edges

updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges path p edges = foldr (\(E x w y) -> updateEdge x y p) edges path

addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow x y p sol
    | length [E s w d | (E s w d) <- sol, s == x, d == y] == 1          = [E s (w+p) d | (E s w d) <- sol, s == x, d == y, w+p /= 0] ++ [E s w d | (E s w d) <- sol, not(s == x && d == y)]
    | length [E s w d | (E s w d) <- sol, s == y, d == x, w == p] == 1  = [E s w d | (E s w d) <- sol, not(s == y && d == x)]
    | length [E s w d | (E s w d) <- sol, s == y, d == x, w < p] == 1   = [E d (p-w) s | (E s w d) <- sol, s == y, d == x] ++ [E s w d | (E s w d) <- sol, not(s == y && d == x)]
    | length [E s w d | (E s w d) <- sol, s == y, d == x, w > p] == 1   = [E s (w-p) d | (E s w d) <- sol, s == y, d == x] ++ [E s w d | (E s w d) <- sol, not(s == y && d == x)]
    | otherwise                                                         = E x p y : sol 

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
                        reversePath = map (\(E x w y) -> E y w x) path
                        mf = maxFlowPath path
                        edges = weightedDiEdges wdg
                        edges' = updateEdges path (-mf) edges 
                        edges'' = updateEdges reversePath mf edges'
                        wdg' = mkWeightedDiGraphEdges (vertices wdg) edges''
                        sol' = addFlows path mf sol


maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow sol src = sum [w | (E x w y) <- sol, x == src]

maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst set = sum [w | (E x w y) <- weightedDiEdges g, x `elem` set, y `notElem` set] - sum [w | (E x w y) <- weightedDiEdges g, x `notElem` set, y `elem` set]

-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial 
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium = undefined

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks = undefined

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink = undefined

