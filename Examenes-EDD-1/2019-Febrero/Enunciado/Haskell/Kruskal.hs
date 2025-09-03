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
kruskal = undefined

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined
-- se hace igual que kruskal solo que añadir en el caso en que es el mismo su propio representante 
-- (creo) y cambiar la salida para que sea lista de listas de aristas 
