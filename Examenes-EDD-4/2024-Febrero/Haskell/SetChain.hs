-- Student's name: Paco Molina Cuenca
-- Student's group: 
-- Identity number (DNI if Spanish/passport if Erasmus): 
 
module SetChain ( 
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
isEmpty (SC mempool history epoch) = S.isEmpty mempool && D.isEmpty history && epoch == 0

-- * Exercise c) 
getEpoch :: (Eq a) => a -> SetChain a -> Integer 
getEpoch trans (SC _ history _) = getEpochRec (D.keysValues history)
  where 
    getEpochRec []                    = -1
    getEpochRec ((epoch, mempool):xs)
      | S.isElem trans mempool = epoch 
      | otherwise              = getEpochRec xs
 
-- * Exercise d) 
size :: SetChain a -> Int 
size (SC _ history _) = sum [S.size mempool | mempool <- D.values history]
 
-- * Exercise e) 
pendingTransactions :: SetChain a -> Bool 
pendingTransactions (SC mempool _ _) = not(S.isEmpty mempool)
 
-- * Exercise f) 
add :: (Eq a) => a -> SetChain a -> SetChain a 
add trans (SC mempool history epoch) = SC (S.insert trans mempool) history epoch
 
-- * Exercise g) 
validate :: (Ord a) => SetChain a -> SetChain a 
validate sc@(SC mempool history epoch)
  | not(pendingTransactions sc)  = sc 
  | alreadyValidatedTrans        = error "transaction already validated"
  | otherwise                    = SC S.empty (D.insert epoch mempool history) (epoch + 1)
  where 
    transList = S.fold (:) [] mempool
    alreadyValidatedTrans = or [S.isElem trans validatedTrans | trans <- transList, let validatedTrans = foldr S.union S.empty (D.values history)]
 
-- * Exercise h) 
addAll :: (Eq a) => [a] -> SetChain a -> SetChain a 
addAll transList sc = foldr add sc transList
 
-- * Exercise i) 
fold :: (Ord a) => (a -> b -> b) -> b -> SetChain a -> b 
fold f z (SC _ history _) = S.fold f z validatedTrans
  where 
    validatedTrans = foldr S.union S.empty (D.values history)
        
-- * Exercise j) 
toList :: (Ord a) => SetChain a -> [a] 
toList = fold (:) []

-- ------------------------------- 
-- DO NOT MODIFY THE CODE BELOW 
-- ------------------------------- 
 
instance (Show a) => Show (SetChain a) where 
  show (SC s h e) = concat ["SetChain(", show s, ", ", show h, ", ", show e, ")"] 
 
instance (Eq a) => Eq (SetChain a) where 
  (SC m1 h1 e1) == (SC m2 h2 e2) = m1 == m2 && h1 == h2 && e1 == e2
