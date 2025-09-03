-------------------------------------------------------------------------------
-- Student's name: Paco Molina Cuenca
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática
--
-- Data Structures. Escuela Térnica Superior de Ingenieria Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle (isEulerian, remove, extractCycle, connectCycles, vertexInCommon, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = foldr (&&) True (map (\x -> (degree g x) `mod` 2 == 0) (vertices g))

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v1,v2) = deleteIsolated (deleteEdge g (v1,v2)) (vertices g)
    where 
        deleteIsolated :: (Eq a) => Graph a -> [a] -> Graph a
        deleteIsolated g [] = g
        deleteIsolated g (v:vs) = 
            if degree g v == 0
                then deleteIsolated (deleteVertex g v) vs 
                else deleteIsolated g vs

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = extractCycle' g v0 [v0]
    where 
        extractCycle' g v path 
            | v == v0 && length path > 1 = (g, reverse path)
            | otherwise = 
                let u = head (successors g v)
                    g' = remove g (v,u)
                    path' = u : path
                in extractCycle' g' u path'
   
-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys 
connectCycles (x:xs) (y:ys)
    | x == y =  x:ys ++ xs
    | otherwise = x : connectCycles xs (y:ys)

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g (x:xs) =
    if x `elem` (vertices g)
        then x 
        else vertexInCommon g xs

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g 
    | not(isEulerian g) = error "eulerianCycle on not eulerian graph"
    | otherwise = eulerianCycle' g verticeArbitrario []
    where 
        verticeArbitrario = head (vertices g)

        eulerianCycle' g v path
            | isEmpty g = path
            | otherwise = 
                let (g', ciclo) = extractCycle g v
                in eulerianCycle' g' (vertexInCommon g' ciclo) (connectCycles path ciclo)
