-------------------------------------------------------------------------------
-- Student's name: Paco Molina Cuenca
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática
--
-- Data Structures. E.T.S.I. de Informática. UMA.
-------------------------------------------------------------------------------

module SparseVector( Vector
                   , vector
                   , size
                   , get
                   , set
                   , mapVector
                   , filterVector
                   , depthOf
                   , pretty
                   ) where

import Test.QuickCheck hiding (vector)

data Tree a = Unif a | Node (Tree a) (Tree a) deriving Show

data Vector a = V Int (Tree a) deriving Show

-------------------------------------------------------------------------------
-- README FIRST
--
-- For each function below, write its TYPE and correspoding DEFINITION.
--
-- Note that there exists different examples to test your solutions below.
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Exercise a.

vector :: Int -> a -> Vector a
vector n x 
  | n < 0 = error "n es negativo"
  | otherwise = V (2^n) (Unif x)

-------------------------------------------------------------------------------
-- | Exercise b.

size :: Vector a -> Int 
size (V tam _) = tam

-------------------------------------------------------------------------------
-- | Exercise c.

simplify :: (Eq a) => Tree a -> Tree a -> Tree a  
simplify lt@(Unif x) rt@(Unif y) = if x == y then Unif x else Node lt rt 
simplify lt rt = Node lt rt

-------------------------------------------------------------------------------
-- | Exercise d.

get :: Int -> Vector a -> a
get i (V tam arbol)
  | i < 0 || i > tam - 1 = error "Indice fuera de rango"
  | otherwise = buscarArbol i tam arbol
  where 
    buscarArbol :: Int -> Int -> Tree a -> a 
    buscarArbol _ _ (Unif x) = x 
    buscarArbol i tam (Node lt rt)
      | i <= (tam `div` 2) - 1 = buscarArbol i (tam `div` 2) lt 
      | otherwise = buscarArbol (i - (tam `div` 2)) (tam `div` 2) rt -- tam/2 <= i <= tam - 1 ==> 0 <= i - tam/2 <= tam/2 - 1

-------------------------------------------------------------------------------
-- | Exercise e.

set :: (Eq a) => Int -> a -> Vector a -> Vector a 
set i x (V tam arbol)
  | i < 0 || i > tam - 1 = error "Indice fuera de rango"
  | otherwise = V tam (setArbol i tam x arbol)
  where 
    setArbol :: (Eq a) => Int -> Int -> a -> Tree a -> Tree a 
    setArbol i tam x (Unif y)
      | tam == 1 = Unif x 
      | i <= (tam `div` 2) = simplify (setArbol i (tam `div` 2) x (Unif y)) (Unif y)
      | otherwise = simplify (Unif y) (setArbol (i - (tam `div` 2)) (tam `div` 2) x (Unif y))
    setArbol i tam x (Node lt rt)
      | i <= (tam `div` 2) - 1 = simplify (setArbol i (tam `div` 2) x lt) rt
      | otherwise = simplify lt (setArbol (i - (tam `div` 2)) (tam `div` 2) x rt)

-------------------------------------------------------------------------------
-- | Exercise f.

-- Fill the table below:
--
--    operation        complexity class
--    ---------------------------------
--    vector                       O(1)
--    ---------------------------------
--    size                         O(1)
--    ---------------------------------
--    simplify                     O(1)
--    ---------------------------------
--    get                        O(log n) 
--    ---------------------------------
--    set                        O(log n)
--    ---------------------------------

-------------------------------------------------------------------------------
-- | Exercise g.

mapVector :: (Eq a, Eq b) => (a -> b) -> Vector a -> Vector b 
mapVector f (V tam arbol) = V tam (mapearArbol f arbol)
  where 
    mapearArbol :: (Eq a, Eq b) => (a -> b) -> Tree a -> Tree b
    mapearArbol f (Unif x) = Unif (f x)
    mapearArbol f (Node lt rt) = simplify (mapearArbol f lt) (mapearArbol f rt)

-------------------------------------------------------------------------------
-- | Exercise h.

filterVector :: (a -> Bool) -> Vector a -> [a]
filterVector p (V tam arbol) = filtroArbol tam arbol
  where 
    filtroArbol tam (Unif x)
      | p x = replicate tam x
      | otherwise = []
    filtroArbol tam (Node lt rt) = filtroArbol (tam `div` 2) lt ++ filtroArbol (tam `div` 2) rt

-------------------------------------------------------------------------------
-- | Exercise i.

depthOf :: Int -> Vector a -> Int 
depthOf i vector@(V tam arbol) = buscarNivelArbol i tam 0 arbol 
  where 
    buscarNivelArbol :: Int -> Int -> Int -> Tree a -> Int 
    buscarNivelArbol _ _ acc (Unif x) = acc 
    buscarNivelArbol i tam acc (Node lt rt)
      | i <= (tam `div` 2) - 1 = buscarNivelArbol i (tam `div` 2) (acc + 1) lt 
      | otherwise = buscarNivelArbol (i - (tam `div` 2)) (tam `div` 2) (acc + 1) rt

-------------------------------------------------------------------------------
-- Sample vectors to test your code. Do not modify them.

vector1 :: Vector Char
vector1 = vector 3 'a'

vector2 :: Vector Char
vector2 = V 8 (Node (Unif 'a') (Unif 'b'))

vector3 :: Vector Char
vector3 = V 8 (Node (Node (Node (Unif 'a') (Unif 'b')) (Node (Unif 'c') (Unif 'd')))
              (Node (Node (Unif 'e') (Unif 'f')) (Node (Unif 'g') (Unif 'h'))))

vector4 :: Vector Char
vector4 = V 8 (Node (Node (Unif 'a') (Node (Unif 'b') (Unif 'c'))) (Unif 'd'))

vector5 :: Vector Char
vector5 = V 4 (Node (Unif 'a') (Node (Unif 'a') (Unif 'b')))

{-
-------------------------------------------------------------------------------
-- Examples for testing your solutions

-------------------------------------------------------------------------------
-- | Exercise a. vector

λ> vector 3 'a'
V 8 (Unif 'a')

λ> vector 0 5
V 1 (Unif 5)

-------------------------------------------------------------------------------
-- | Exercise b. size

λ> size vector1
8

λ> size vector5
4

-------------------------------------------------------------------------------
-- | Exercise c. simplify

λ> simplify (Unif 4) (Unif 4)
Unif 4

λ> simplify (Unif 4) (Unif 2)
Node (Unif 4) (Unif 2)

λ> simplify (Node  (Unif 3) (Unif 4)) (Node (Unif 3) (Unif 4))
Node (Node (Unif 3) (Unif 4)) (Node (Unif 3) (Unif 4))

-------------------------------------------------------------------------------
-- | Exercise d. get

λ> [ get i vector1 | i <- [0..size vector1 - 1] ]
"aaaaaaaa"

λ> [ get i vector2 | i <- [0..size vector2 - 1] ]
"aaaabbbb"

λ> [ get i vector3 | i <- [0..size vector3 - 1] ]
"abcdefgh"

λ> [ get i vector4 | i <- [0..size vector4 - 1] ]
"aabcdddd"

λ> [ get i vector5 | i <- [0..size vector5 - 1] ]
"aaab"

-------------------------------------------------------------------------------
-- | Exercise e. set

The original vector4 is:

λ> pretty vector4
    ________
   /        \
   ____     'd'
  /    \
'a'    _
      / \
    'b' 'c'

We now set some of its elements:

λ> pretty (set 0 'x' vector4)
        ________
       /        \
    _______     'd'
   /       \
   _       _
  / \     / \
'x' 'a' 'b' 'c'

λ> pretty (set 3 'b' vector4)
    ____
   /    \
   _    'd'
  / \
'a' 'b'

λ> pretty (set 5 'c' vector4)
    _______________
   /               \
   ____         ____
  /    \       /    \
'a'    _       _    'd'
      / \     / \
    'b' 'c' 'd' 'c'

λ> pretty (set 4 'c' vector4)
    _______________
   /               \
   ____         ____
  /    \       /    \
'a'    _       _    'd'
      / \     / \
    'b' 'c' 'c' 'd'


The original vector5 is:

λ> pretty vector5
   ____
  /    \
'a'    _
      / \
    'a' 'b'

After setting its fourth element to 'a' we get:

λ> pretty (set 3 'a' vector5)
'a'

-------------------------------------------------------------------------------
-- | Exercise g. mapVector

The original vector3 is:

λ> pretty vector3
        _______________
       /               \
    _______         _______
   /       \       /       \
   _       _       _       _
  / \     / \     / \     / \
'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h'

We now map some operations to it:

λ> pretty (mapVector (:"s") vector3)
          ___________________
         /                   \
     _________           _________
    /         \         /         \
    _         _         _         _
   / \       / \       / \       / \
"as" "bs" "cs" "ds" "es" "fs" "gs" "hs"

λ> pretty (mapVector (`elem` "aeiou") vector3)
           ________________
          /                \
     ______           ______
    /      \         /      \
    _      False     _      False
   / \              / \
True False       True False

λ> pretty (mapVector (\ _ -> 'a') vector3)
'a'

-------------------------------------------------------------------------------
-- | Exercise h. filterVector

λ> filterVector (`elem` "aeiou") vector3
"ae"

λ> filterVector (/= 'a') vector5
"b"

λ> filterVector (== 'a') vector5
"aaa"

λ> filterVector (\ _ -> True) vector5
"aaab"

-------------------------------------------------------------------------------
-- | Exercise i. depthOf

λ> [ depthOf i vector1 | i <- [0..size vector1 - 1] ]
[0,0,0,0,0,0,0,0]

λ> [ depthOf i vector2 | i <- [0..size vector2 - 1] ]
[1,1,1,1,1,1,1,1]

λ>> [ depthOf i vector3 | i <- [0..size vector3 - 1] ]
[3,3,3,3,3,3,3,3]

λ> [ depthOf i vector4 | i <- [0..size vector4 - 1] ]
[2,2,3,3,1,1,1,1]

λ> [ depthOf i vector5 | i <- [0..size vector5 - 1] ]
[1,1,2,2]

-}

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

maxExp = 9 :: Int

instance (Eq a, Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = do
    sz <- elements [0..maxExp]
    x <- arbitrary
    xs <- arbitrary
    is <- listOf (elements [0..2^sz-1])
    return $ foldr set' ((vector :: Int -> a -> Vector a) sz x) (zip is xs)
    where
      set' :: Eq a => (Int, a) -> Vector a -> Vector a
      set' = uncurry set

-------------------------------------------------------------------------------
-- Pretty Printing a Vector
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

pretty :: (Show a) => Vector a -> IO ()
pretty (V _ t)  = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint t

pprint (Unif x)              = ([s], ls, 0, ls-1)
  where
    s = show x
    ls = length s
pprint (Node lt rt)         =  (resultLines, w, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = ""
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
