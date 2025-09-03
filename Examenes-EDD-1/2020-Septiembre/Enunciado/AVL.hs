{------------------------------------------------------------------------------
 - Student's name: Paco Molina Cuenca
 -
 - Student's group: Doble Grado Matemáticas e Ingeniería Informática.
 -----------------------------------------------------------------------------}

module AVL 
  ( 
    Weight
  , Capacity
  , AVL (..)
  , Bin
  , emptyBin
  , remainingCapacity
  , addObject
  , maxRemainingCapacity
  , height
  , nodeWithHeight
  , node
  , rotateLeft
  , addNewBin
  , addFirst
  , addAll
  , toList
  , linearBinPacking
  , seqToList
  , addAllFold
  ) where

type Capacity = Int
type Weight= Int

data Bin = B Capacity [Weight] 

data AVL = Empty | Node Bin Int Capacity AVL AVL deriving Show


emptyBin :: Capacity -> Bin
emptyBin = undefined

remainingCapacity :: Bin -> Capacity
remainingCapacity = undefined

addObject :: Weight -> Bin -> Bin
addObject = undefined

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity = undefined

height :: AVL -> Int
height = undefined

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight = undefined

node :: Bin -> AVL -> AVL -> AVL
node = undefined

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft = undefined

addNewBin :: Bin -> AVL -> AVL
addNewBin = undefined
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst = undefined

addAll:: Capacity -> [Weight] -> AVL
addAll = undefined

toList :: AVL -> [Bin]
toList = undefined

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
-}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking _ _ = undefined

seqToList:: Sequence -> [Bin]
seqToList _ = undefined

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold _ _ = undefined



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
