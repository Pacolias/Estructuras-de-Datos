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
kruskal wg = kruskalRec (foldr (\x -> D.insert x x) D.empty (vertices wg)) (foldr (\x -> Q.enqueue x) Q.empty (edges wg)) []
    where 
        kruskalRec dict queue t
            | Q.isEmpty queue = t 
            | otherwise = 
                if representante src /= representante dst 
                    then kruskalRec (D.updateOrInsert dst (const src) src dict) (Q.dequeue queue) (cabezaCola : t)
                    else kruskalRec dict (Q.dequeue queue) t
                where 
                    cabezaCola = Q.first queue 
                    (WE src w dst) = Q.first queue

                    representante vertice = 
                        case D.valueOf vertice dict of 
                            Nothing -> error "valueOf on key where dictionary is not defined"
                            Just v -> if vertice == v then vertice else representante v
                            

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined