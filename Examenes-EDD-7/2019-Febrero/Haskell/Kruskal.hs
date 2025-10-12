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
            | otherwise    = kruskalRec dict' pq' t' 
                where 
                    edge@(WE org w dst) = Q.first pq 
                    repOrg = representante org 
                    repDst = representante dst 
                    dict'  = if repOrg /= repDst then D.updateOrInsert repDst (const org) org dict else dict
                    pq'    = Q.dequeue pq 
                    t'     = if repOrg /= repDst then edge : t else t

                    representante v = 
                        case D.valueOf v dict of 
                            Nothing -> error ""
                            Just u  -> if u == v then u else representante u


-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined
-- se hace igual que kruskal solo que añadir en el caso en que es el mismo su propio representante 
-- (creo) y cambiar la salida para que sea lista de listas de aristas 
