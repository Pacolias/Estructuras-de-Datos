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
maxFlowPath lista = minimum (map weightEdge lista)
    where 
        weightEdge :: WDiEdge a Integer -> Integer 
        weightEdge (E a peso b) = peso

updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge x y p = map actualizaArco 
    where 
        actualizaArco e@(E o peso d)
            | o == x && d == y = E x p y
            | otherwise = e

updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges [] _ edges = edges 
updateEdges (e@(E o p d):es) peso edges = updateEdges es peso (updateEdge o d peso edges)

addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow x y p sol 
    | wInv /= 0 = updateEdge x y (p + wInv) sol 
    | w /= 0 && w == p = deleteEdge y x sol 
    | w /= 0 && w < p = (E x (p - w) y) : (deleteEdge y x sol)
    | w /= 0 && w > p = updateEdge y x (w - p) sol 
    | otherwise = (E x p y) : sol
    where 
        w = existeArco y x sol
        wInv = existeArco x y sol

        existeArco :: (Eq a) => a -> a -> [WDiEdge a Integer] -> Integer
        existeArco _ _ [] = 0 
        existeArco x y (e@(E o peso d):es)
            | x == o && y == d = peso
            | otherwise = existeArco x y es
        
        deleteEdge :: (Eq a) => a -> a -> [WDiEdge a Integer] -> [WDiEdge a Integer]
        deleteEdge x y [] = []
        deleteEdge x y (e@(E o _ d):es)
            | x == o && y == d = es 
            | otherwise = e : (deleteEdge x y es)

addFlows :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlows [] _ sol = sol 
addFlows (e@(E x peso y):es) p sol = addFlows es p (addFlow x y p sol) 

fordFulkerson :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer]
fordFulkerson g src dst = fordFulkersonRec [] g 
    where 
        fordFulkersonRec sol wdg = 
            case bftPathTo wdg src dst of 
                Nothing     -> sol 
                Just path   -> fordFulkersonRec sol' wdg'
                    where 
                        mf = maxFlowPath path
                        edges = weightedDiEdges wdg
                        edges' = updateEdges path (-mf) edges
                        edges'' = updateEdges (reversePath path) mf edges'
                        wdg' = mkWeightedDiGraphEdges (vertices wdg) edges''
                        sol' = addFlows path mf sol

                        reversePath :: Path (WDiEdge a Integer) -> Path (WDiEdge a Integer)
                        reversePath [] = []
                        reversePath (e@(E x peso y):es) = (E y peso x) : (reversePath es)

maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow sol src = flujoMaximoRec sol 
    where 
        flujoMaximoRec [] = 0 
        flujoMaximoRec (e@(E x p y):es)
            | x == src = p + (flujoMaximoRec es)
            | otherwise = flujoMaximoRec es

maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut wdg src dst set = sum [p | E x p y <- aristas, x `elem` set, y `notElem` set] - sum [p | E x p y <- aristas, y `elem` set, x `notElem` set]
    where 
        aristas = weightedDiEdges wdg

-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial 
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium wdg src dst = comprobacionEquilibrioRec (vertices wdg)
    where 
        comprobacionEquilibrioRec [] = True 
        comprobacionEquilibrioRec (v:vs) 
            | v == src || v == dst = comprobacionEquilibrioRec vs
            | otherwise = (sum [p | E x p y <- aristas, x == v] == sum [p | E x p y <- aristas, y == v]) && comprobacionEquilibrioRec vs
                where 
                    aristas = weightedDiEdges wdg

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks wdg = ([v | v <- vertices wdg, inDegree wdg v == 0], [v | v <- vertices wdg, outDegree wdg v == 0])

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink wdg newSrc newDst
    | length fuentes == 1 && length sumideros == 1 = wdg 
    | length fuentes > 1 && length sumideros == 1 = mkWeightedDiGraphEdges (newSrc : (vertices wdg)) ((addFuentes fuentes) ++ (weightedDiEdges wdg))
    | length fuentes == 1 && length sumideros > 1 = mkWeightedDiGraphEdges (newDst : (vertices wdg)) ((addSumideros sumideros) ++ (weightedDiEdges wdg))
    | otherwise = mkWeightedDiGraphEdges (newSrc : newDst : (vertices wdg)) ((addFuentes fuentes) ++ (addSumideros sumideros) ++ (weightedDiEdges wdg))
    where 
        (fuentes, sumideros) = sourcesAndSinks wdg

        addFuentes [] = []
        addFuentes (f:fs) = (E newSrc peso f) : (addFuentes fs)
            where 
                peso = sum [p | E x p _ <- weightedDiEdges wdg, x == f]

        addSumideros [] = []
        addSumideros (s:ss) = (E s peso newDst) : (addSumideros ss)
            where 
                peso = sum [p | E _ p y <- weightedDiEdges wdg, y == s]
