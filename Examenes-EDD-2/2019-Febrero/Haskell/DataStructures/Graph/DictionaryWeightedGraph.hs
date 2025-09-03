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
-- Weighted Graph implemented by using a dictionary from
-- sources to another dictionary from destinations to weights
----------------------------------------------

module DataStructures.Graph.DictionaryWeightedGraph
  ( WeightedGraph
  , WeightedEdge(WE)
  , empty
  , isEmpty
  , mkWeightedGraphEdges
  , addVertex
  , addEdge
  , vertices
  , numVertices
  , edges
  , numEdges
  , successors
  ) where

import Data.List(nub, intercalate)

import qualified DataStructures.Dictionary.AVLDictionary as D

data WeightedEdge a w  = WE a w a deriving Show

instance (Eq a, Eq w) => Eq (WeightedEdge a w) where
  WE u w v == WE u' w' v' = (u==u' && v==v' || u==v' && v==u')
                              && w == w'

instance (Eq a, Ord w) => Ord (WeightedEdge a w) where
  compare (WE _ w _) (WE _ w' _) = compare w w'

data WeightedGraph a w  = WG (D.Dictionary a (D.Dictionary a w))

empty :: WeightedGraph a w
empty = WG D.empty

addVertex :: (Ord a) => WeightedGraph a w -> a -> WeightedGraph a w
addVertex (WG dict) v = WG (D.insert v D.empty dict)

addEdge :: (Ord a, Show a) => WeightedGraph a w -> a -> a -> w -> WeightedGraph a w
addEdge wg@(WG dict) x y p
  | x `notElem` vertices wg || y `notElem` vertices wg = error "addEdge on graph not defined on the given vertices"
  | otherwise = 
    case D.valueOf x dict of 
      Nothing         -> error "addEdge on graph not defined on the given vertices"
      Just valueDict  -> WG (D.updateOrInsert x (const nuevoDict) nuevoDict dict)
        where 
          nuevoDict = D.updateOrInsert y (const p) p valueDict

edges :: (Ord a, Show a, Eq a, Eq w) => WeightedGraph a w -> [WeightedEdge a w]
edges wg = map (\(x,p,y) -> WE x p y) (foldr (\v acc -> toTriplets v (successors wg v) ++ acc) [] (vertices wg))
  where 
    toTriplets x [] = []
    toTriplets x ((y,p):xs) = (x,p,y) : toTriplets x xs

successors :: (Ord a, Show a) => WeightedGraph a w -> a -> [(a,w)]
successors wg@(WG dict) v 
  | v `notElem` vertices wg = error "addEdge on graph not defined on the given vertex"
  | otherwise               = maybe [] D.keysValues (D.valueOf v dict)

-- NO EDITAR A PARTIR DE AQUÍ    
-- DON'T EDIT ANYTHING BELOW THIS COMMENT

vertices :: WeightedGraph a w -> [a]
vertices (WG d) = D.keys d

isEmpty :: WeightedGraph a w -> Bool
isEmpty (WG d) = D.isEmpty d

mkWeightedGraphEdges :: (Ord a, Show a) => [a] -> [WeightedEdge a w] -> WeightedGraph a w
mkWeightedGraphEdges vs es = wg'
  where
    wg = foldl addVertex empty vs
    wg' = foldr (\(WE u w v) wg -> addEdge wg u v w) wg es

numVertices :: WeightedGraph a w -> Int
numVertices = length . vertices

numEdges :: (Ord a, Show a, Eq a, Eq w) => WeightedGraph a w -> Int
numEdges = length . edges

instance (Ord a, Eq a, Show a, Eq w, Show w) => Show (WeightedGraph a w) where
  show wg  = "DictionaryWeightedGraph("++vs++", "++as++")"
   where
    vs  = "("++ intercalate ", " (map show (vertices wg)) ++")"
    as  = "(" ++ intercalate ", " (map showEdge (edges wg)) ++ ")"
    showEdge (WE x w y)  = intercalate "-" [ show x, show w, show y ]
