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

-- (A) Devuelve el grafo inverso
reverseDiGraph :: Eq a => DiGraph a -> DiGraph a
reverseDiGraph = undefined

-- (B) Subgrafo con los vértices dados
restrictDiGraph :: Eq a => DiGraph a -> [a] -> DiGraph a
restrictDiGraph = undefined

-- (C) SCC de un vértice
type SCC a = [a]

sccOf :: Ord a => DiGraph a -> a -> SCC a
sccOf = undefined

-- (D) Todas las componentes fuertemente conexas
sccs :: Ord a => DiGraph a -> [SCC a]
sccs = undefined

