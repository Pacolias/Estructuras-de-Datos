-- Examen Febrero 2014 - SCCs
-- Data Structures. UMA

module SCCs (
    reverseDiGraph,
    restrictDiGraph,
    sccOf,
    sccs
) where

import DataStructures.Graph.DiGraph
import DataStructures.Graph.DiGraphDFT (dft)
import qualified DataStructures.Set.BSTSet as S
import Data.List ((\\))

-- (A) Devuelve el grafo inverso
reverseDiGraph :: Eq a => DiGraph a -> DiGraph a
reverseDiGraph g = mkDiGraphEdges (vertices g) [v :-> u | u :-> v <- diEdges g]

-- (B) Subgrafo con los vértices dados
restrictDiGraph :: Eq a => DiGraph a -> [a] -> DiGraph a
restrictDiGraph g vs = mkDiGraphEdges vs [u :-> v | u :-> v <- diEdges g, u `elem` vs, v `elem` vs]

-- (C) SCC de un vértice
type SCC a = [a]

sccOf :: Ord a => DiGraph a -> a -> SCC a
sccOf g v = dft g' v
    where 
        vs = dft g v 
        gr = restrictDiGraph g vs 
        g' = reverseDiGraph gr

-- (D) Todas las componentes fuertemente conexas
sccs :: Ord a => DiGraph a -> [SCC a]
sccs g = sccsRec (vertices g)
    where 
        sccsRec []     = []
        sccsRec (v:vs) = scc : sccsRec (vs \\ scc)
            where 
                scc = sccOf g v
