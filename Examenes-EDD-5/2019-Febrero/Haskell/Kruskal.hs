----------------------------------------------
-- Estructuras de Datos.  2018/19
-- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
-- Escuela Técnica Superior de Ingeniería en Informática. UMA
--
-- Examen 4 de febrero de 2019
--
-- ALUMNO/NAME: Paco Molina Cuenca
-- GRADO/STUDIES: Doble Grado Matemáticas e Ingeniería Infomática
-- NÚM. MÁQUINA/MACHINE NUMBER:
--
----------------------------------------------

module Kruskal(kruskal, kruskals) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.PriorityQueue.LinearPriorityQueue as Q
import DataStructures.Graph.DictionaryWeightedGraph

kruskal :: (Ord a, Ord w) => WeightedGraph a w -> [WeightedEdge a w]
kruskal g = kruskalRec (foldr (\v -> D.insert v v) D.empty (vertices g)) (foldr Q.enqueue Q.empty (edges g)) []
    where 
        kruskalRec dict pq t 
            | Q.isEmpty pq = t 
            | otherwise    = if repOrg /= repDst then kruskalRec (D.updateOrInsert repDst (const org) org dict) (Q.dequeue pq) (edge:t) else kruskalRec dict (Q.dequeue pq) t
                where
                    edge@(WE org w dst) = Q.first pq  
                    repOrg = representante org 
                    repDst = representante dst

                    representante x =
                        case D.valueOf x dict of 
                            Nothing -> error ""
                            Just y  -> if x == y then x else representante y

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined