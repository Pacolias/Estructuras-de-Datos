module ERank where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.Graph.Graph as G

threshold :: Double
threshold = 0.00001

createDict :: (Ord a) => G.Graph a -> D.Dictionary a Double
createDict g = undefined

distribute :: Ord a => Double -> a -> G.Graph a -> D.Dictionary a Double
distribute value v g = distributeaux value v g (createDict g)

distributeaux :: Ord a => Double -> a -> G.Graph a -> D.Dictionary a Double -> D.Dictionary a Double
distributeaux value vertex graph dict = undefined

distributelist :: Ord a => [a] -> Double -> G.Graph a -> D.Dictionary a Double -> D.Dictionary a Double
distributelist = undefined

erank :: Ord a => Double -> G.Graph a -> D.Dictionary a Double
erank value g = distributelist (G.vertices g) value g (createDict g)
