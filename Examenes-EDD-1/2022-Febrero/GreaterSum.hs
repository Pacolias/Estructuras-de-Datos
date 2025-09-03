module GreaterSum where

import DataStructures.SearchTree.BST (BST(..))

greaterSum :: BST Int -> BST Int
greaterSum t = fst (aux t 0)
  where
    aux :: BST Int -> Int -> (BST Int, Int)
    aux Empty acc = (Empty, acc)
    aux (Node x l r) acc = undefined
