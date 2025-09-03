-- | Data Structures
-- | September, 2016
-- |
-- | Student's name: Paco Molina Cuenca
-- | Student's group: Doble Grado Matemáticas e igeniería informática

module Huffman where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue as PQ
import Data.List (nub)

-- | Exercise 1

weights :: Ord a => [a] -> D.Dictionary a Int
weights lista = foldr (\x -> D.insert x (length [y | y <- lista, y == x])) D.empty lista
    
{-

> weights "abracadabra"
AVLDictionary('a'->5,'b'->2,'c'->1,'d'->1,'r'->2)

> weights [1,2,9,2,0,1,6,1,5,5,8]
AVLDictionary(0->1,1->3,2->2,5->2,6->1,8->1,9->1)

> weights ""
AVLDictionary()

-}


-- Implementation of Huffman Trees
data WLeafTree a = WLeaf a Int  -- Stored value (type a) and weight (type Int)
                 | WNode (WLeafTree a) (WLeafTree a) Int -- Left child, right child and weight
                 deriving (Eq, Show)

weight :: WLeafTree a -> Int
weight (WLeaf _ n)   = n
weight (WNode _ _ n) = n

-- Define order on trees according to their weights
instance Eq a => Ord (WLeafTree a) where
  wlt <= wlt' =  weight wlt <= weight wlt'

-- Build a new tree by joining two existing trees
merge :: WLeafTree a -> WLeafTree a -> WLeafTree a
merge wlt1 wlt2 = WNode wlt1 wlt2 (weight wlt1 + weight wlt2)


-- | Exercise 2

-- 2.a
huffmanLeaves :: String -> PQ.PQueue (WLeafTree Char)
huffmanLeaves palabra = foldr (\(x,y) -> PQ.enqueue (WLeaf x y)) PQ.empty (D.keysValues (weights palabra))

{-

> huffmanLeaves "abracadabra"
WBLeftistHeapPriorityQueue(WLeaf 'c' 1,WLeaf 'd' 1,WLeaf 'b' 2,WLeaf 'r' 2,WLeaf 'a' 5)

-}

-- 2.b
huffmanTree :: String -> WLeafTree Char
huffmanTree palabra
  | length (D.keysValues (weights palabra)) < 2 = error "La palabra pasada como parametro no tiene al menos 2 caracteres distintos"
  | otherwise = construirArbol (huffmanLeaves palabra)
    where 
      construirArbol :: PQ.PQueue (WLeafTree Char) -> WLeafTree Char 
      construirArbol colaPalabra 
        | PQ.isEmpty (PQ.dequeue colaPalabra) = primero
        | otherwise = construirArbol (PQ.enqueue mezclaPrimeroSegundo colaParametro)
          where 
            primero = PQ.first colaPalabra
            segundo = PQ.first (PQ.dequeue colaPalabra)
            mezclaPrimeroSegundo = merge primero segundo
            colaParametro = PQ.dequeue (PQ.dequeue colaPalabra)

{-

> printWLeafTree $ huffmanTree "abracadabra"
      11_______
      /        \
('a',5)        6_______________
              /                \
        ('r',2)          _______4
                        /       \
                       2        ('b',2)
                      / \
                ('c',1) ('d',1)

> printWLeafTree $ huffmanTree "abracadabra pata de cabra"
                         ______________________25_____________
                        /                                     \
         ______________10______                          ______15
        /                      \                        /       \
       4_______                6_______                6        ('a',9)
      /        \              /        \              / \
('d',2)        2        (' ',3)        3        ('b',3) ('r',3)
              / \                     / \
        ('e',1) ('p',1)         ('t',1) ('c',2)

> printWLeafTree $ huffmanTree "aaa"
*** Exception: huffmanTree: the string must have at least two different symbols

-}


-- | Exercise 3

-- 3.a
-- There is no key defined in both dictionaries
joinDics :: Ord a => D.Dictionary a b -> D.Dictionary a b -> D.Dictionary a b 
joinDics dict1 dict2 = unionDisjunta ((D.keys dict1) ++ (D.keys dict2))
  where 
    unionDisjunta [] = D.empty 
    unionDisjunta (k:ks) = 
      case D.valueOf k dict1 of 
        Just v  -> D.insert k v (unionDisjunta ks)
        Nothing ->
          case D.valueOf k dict2 of 
            Just v  -> D.insert k v (unionDisjunta ks)
            Nothing -> unionDisjunta ks

{-

> joinDics (D.insert 'a' 1 $ D.insert 'c' 3 $ D.empty) D.empty
AVLDictionary('a'->1,'c'->3)

> joinDics (D.insert 'a' 1 $ D.insert 'c' 3 $ D.empty) (D.insert 'b' 2 $ D.insert 'd' 4 $ D.insert 'e' 5 $ D.empty)
AVLDictionary('a'->1,'b'->2,'c'->3,'d'->4,'e'->5)

-}

-- 3.b
prefixWith :: Ord a => b -> D.Dictionary a [b] -> D.Dictionary a [b]
prefixWith x dict = prefijarRec (D.keys dict)
  where 
    prefijarRec [] = D.empty 
    prefijarRec (k:ks) = 
      case D.valueOf k dict of 
        Nothing -> prefijarRec ks 
        Just v  -> D.insert k (x:v) (prefijarRec ks)

{-

> prefixWith 0 (D.insert 'a' [0,0,1] $ D.insert 'b' [1,0,0] $ D.empty)
AVLDictionary('a'->[0,0,0,1],'b'->[0,1,0,0])

> prefixWith 'h' (D.insert 1 "asta" $ D.insert 2 "echo" $ D.empty)
AVLDictionary(1->"hasta",2->"hecho")

-}

-- 3.c
huffmanCode :: WLeafTree Char -> D.Dictionary Char [Integer]
huffmanCode (WLeaf x _) = D.insert x [] D.empty
huffmanCode (WNode lt rt _) = joinDics (prefixWith 0 (huffmanCode lt)) (prefixWith 1 (huffmanCode rt))

{-

> huffmanCode (huffmanTree "abracadabra")
AVLDictionary('a'->[0],'b'->[1,1,1],'c'->[1,1,0,0],'d'->[1,1,0,1],'r'->[1,0])

-}

-- ONLY for students not taking continuous assessment

-- | Exercise 4

encode :: String -> D.Dictionary Char [Integer] -> [Integer]
encode palabra dictPalabra = codificar palabra
  where 
    codificar :: String -> [Integer]
    codificar [] = []
    codificar (c:cs) = 
      case D.valueOf c dictPalabra of 
        Nothing     -> codificar cs 
        Just codigo -> codigo ++ (codificar cs)

{-

> encode "abracadabra" (huffmanCode (huffmanTree "abracadabra"))
[0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0]

-}

-- | Exercise 5

-- 5.a
takeSymbol :: [Integer] -> WLeafTree Char -> (Char, [Integer])
takeSymbol codigo (WLeaf x _) = (x, codigo)
takeSymbol (c:cs) (WNode lt rt _) = if c == 0 then takeSymbol cs lt else takeSymbol cs rt

{-

> takeSymbol [0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0] (huffmanTree "abracadabra")
('a',[1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0])

> takeSymbol [1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0] (huffmanTree "abracadabra")
('b',[1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0])

-}

-- 5.b
decode :: [Integer] -> WLeafTree Char -> String
decode [] _ = ""
decode codigo arbol = (primeraComponente simbolo)  ++ (decode (segundaComponente simbolo) arbol)
  where 
    primeraComponente :: (Char, [Integer]) -> String 
    primeraComponente (c, lista) = [c]

    segundaComponente :: (Char, [Integer]) -> [Integer] 
    segundaComponente (c, lista) = lista

    simbolo = takeSymbol codigo arbol

{-

> decode [0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0] (huffmanTree "abracadabra")
"abracadabra"

-}

-------------------------------------------------------------------------------
-- Pretty Printing a WLeafTree
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

printWLeafTree :: (Show a) => WLeafTree a -> IO ()
printWLeafTree t = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint t

pprint :: Show a => WLeafTree a -> ([String], Int, Int, Int)
pprint (WLeaf x we)             =  ([s], ls, 0, ls-1)
  where
    s = show (x,we)
    ls = length s
pprint (WNode lt rt we)         =  (resultLines, w, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show we
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,_,lc) = pprint lt
    (rp,rw,rc,_) = pprint rt
    -- recurse
    (lw',lb) = if lw==0 then (1," ") else (lw,"/")
    (rw',rb) = if rw==0 then (1," ") else (rw,"\\")
    -- compute full width of this tree
    totLW = maximum [lw', swl,  1]
    totRW = maximum [rw', swr, 1]
    w = totLW + 1 + totRW
{-
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totLW
                -----------    totRW
     ----   -   -----------    w (total width)
-}
    -- get right column info that accounts for left side
    rc2 = totLW + 1 + rc
    -- make left and right tree same height
    llp = length lp
    lrp = length rp
    lp' = if llp < lrp then lp ++ replicate (lrp - llp) "" else lp
    rp' = if lrp < llp then rp ++ replicate (llp - lrp) "" else rp
    -- widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
    lp'' = map (\s -> if length s < totLW then nSpaces (totLW - length s) ++ s else s) lp'
    rp'' = map (\s -> if length s < totRW then s ++ nSpaces (totRW - length s) else s) rp'
    -- first part of line1
    line1 = if swl < lw' - lc - 1 then
                nSpaces (lc + 1) ++ nBars (lw' - lc - swl) ++ s
            else
                nSpaces (totLW - swl) ++ s
    -- line1 right bars
    lline1 = length line1
    line1' = if rc2 > lline1 then
                line1 ++ nBars (rc2 - lline1)
             else
                line1
    -- line1 right padding
    line1'' = line1' ++ nSpaces (w - length line1')
    -- first part of line2
    line2 = nSpaces (totLW - lw' + lc) ++ lb
    -- pad rest of left half
    line2' = line2 ++ nSpaces (totLW - length line2)
    -- add right content
    line2'' = line2' ++ " " ++ nSpaces rc ++ rb
    -- add right padding
    line2''' = line2'' ++ nSpaces (w - length line2'')
    resultLines = line1'' : line2''' : zipWith (\lt rt -> lt ++ " " ++ rt) lp'' rp''
