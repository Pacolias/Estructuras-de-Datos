-- Student's name: Paco Molina Cuenca
-- Student's group: 
-- Identity number (DNI if Spanish/passport if Erasmus): 
 
module DataStructures.SetChain.SetChain ( 
    SetChain 
    , empty 
    , isEmpty 
    , pendingTransactions 
    , add 
    , getEpoch 
    , size 
    , validate 
    --  
    , addAll     
    , fold 
    , toList 
    ) where 
 
import qualified DataStructures.Set.LinearSet as S 
import qualified DataStructures.Dictionary.AVLDictionary as D 

--                    Mempool            History                 Epoch 
data SetChain a = SC (S.Set a) (D.Dictionary Integer (S.Set a)) Integer 
 
-- ------------------------------- 
-- DO NOT MODIFY THE CODE ABOVE 
-- ------------------------------- 

-- * Exercise a) 
empty :: SetChain a
empty = SC S.empty D.empty 0

-- * Exercise b) 
isEmpty :: SetChain a -> Bool
isEmpty (SC mempool history epoch) = S.isEmpty mempool && D.isEmpty history

-- * Exercise c) 
getEpoch :: (Eq a) => a -> SetChain a -> Integer 
getEpoch trans (SC mempool history epoch) = getEpochRec (D.keysValues history)
  where 
    getEpochRec [] = -1
    getEpochRec ((ep,set):xs) = if S.isElem trans set then ep else getEpochRec xs
 
-- * Exercise d) 
size :: SetChain a -> Int 
size (SC _ history _) = foldr (+) 0 (map S.size (D.values history))
 
-- * Exercise e) 
pendingTransactions :: SetChain a -> Bool 
pendingTransactions (SC mempool _ _)
  | S.isEmpty mempool = False
  | otherwise         = True
 
-- * Exercise f) 
add :: (Eq a) => a -> SetChain a -> SetChain a 
add trans (SC mempool history epoch) = SC (S.insert trans mempool) history epoch
 
-- * Exercise g) 
validate :: (Eq a, Ord a) => SetChain a -> SetChain a 
validate sc@(SC mempool history epoch)
  | not(pendingTransactions sc)                                                           = sc 
  | not(foldr (&&) True (map (\s -> S.isEmpty (S.intersection mempool s)) (D.values history))) = error "transaction already validated"
  | otherwise                                                                             = SC S.empty (D.insert epoch mempool history) (epoch+1) 
 
-- * Exercise h) 
addAll :: (Eq a) => [a] -> SetChain a -> SetChain a 
addAll listaTrans sc = foldr add sc listaTrans 
 
-- * Exercise i) 
fold :: (a -> b -> b) -> b -> SetChain a -> b 
fold f z sc@(SC _ history _) = foldr (flip (S.fold f)) z (D.values history)
        
-- * Exercise j) 
toList :: SetChain a -> [a] 
toList = fold (:) []

-- ------------------------------- 
-- DO NOT MODIFY THE CODE BELOW 
-- ------------------------------- 
 
instance (Show a) => Show (SetChain a) where 
  show (SC s h e) = concat ["SetChain(", show s, ", ", show h, ", ", show e, ")"] 
 
instance (Eq a) => Eq (SetChain a) where 
  (SC m1 h1 e1) == (SC m2 h2 e2) = m1 == m2 && h1 == h2 && e1 == e2
