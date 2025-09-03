-------------------------------------------------------------------------------
-- Student's name: Paco Molina Cuenca
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática
--
-- Data Structures. Escuela Térnica Superior de Ingenieria Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, remove, extractCycle, connectCycles, vertexInCommon, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = and [even (degree g v) | v <- vertices g]

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g edge = mkGraphEdges [v | v <- vertices g', degree g' v /= 0] (edges g')
    where 
        g' = deleteEdge g edge

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = extractCycleRec g v0 [v0]
    where 
        extractCycleRec g v xs
            | v == v0 && length xs > 1 = (g,xs) 
            | otherwise                = extractCycleRec g' u (xs ++ [u])
                where 
                    u = head (successors g v)
                    g' = remove g (v,u)
   
-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys 
connectCycles (x:xs) (y:ys) 
    | x == y    = (y:ys) ++ xs
    | otherwise = x : connectCycles xs (y:ys)

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g xs = head [v | v <- vertices g, v `elem` xs]

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g
    | not(isEulerian g) = error "eulerianCycle on not eulerian graph"
    | otherwise         = eulerianCycleRec g (head (vertices g)) []
        where 
            eulerianCycleRec g v xs 
                | isEmpty g = xs 
                | otherwise = eulerianCycleRec g' (vertexInCommon g' ys) (connectCycles xs ys)
                    where 
                        (g',ys) = extractCycle g v