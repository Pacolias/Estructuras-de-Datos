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
isEulerian = undefined

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove = undefined

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle = undefined
   
-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles = undefined

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon = undefined

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle = undefined




