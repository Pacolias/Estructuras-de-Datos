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
isEmpty (SC mempool history epoch) = S.isEmpty mempool && D.isEmpty history

-- * Exercise c) 
getEpoch :: (Eq a) => a -> SetChain a -> Integer 
getEpoch transaction (SC _ history _) = searchTransaction (D.keys history)
  where 
    searchTransaction :: [Integer] -> Integer
    searchTransaction [] = -1 
    searchTransaction (k:ks) = 
      case D.valueOf k history of 
        Nothing           -> searchTransaction ks 
        Just transactions -> if S.isElem transaction transactions then k else searchTransaction ks
 
-- * Exercise d) 
size :: SetChain a -> Int 
size (SC _ history _) = sum (map S.size (D.values history))
 
-- * Exercise e) 
pendingTransactions :: SetChain a -> Bool 
pendingTransactions (SC mempool _ _) = not(S.isEmpty mempool)
 
-- * Exercise f) 
add :: (Eq a) => a -> SetChain a -> SetChain a 
add transaction (SC mempool history epoch) = SC (S.insert transaction mempool) history epoch
 
-- * Exercise g) 
validate :: (Ord a) => SetChain a -> SetChain a 
validate setChain@(SC mempool history epoch)
  | not(pendingTransactions setChain) = setChain
  | foldr (||) False (map (\set -> not(S.isEmpty (S.intersection mempool set))) (D.values history)) == True = error "transaction already validated"
  | otherwise = SC S.empty (D.insert epoch mempool history) (epoch+1)
 
-- * Exercise h) 
addAll :: (Eq a) => [a] -> SetChain a -> SetChain a 
addAll transactions setChain@(SC mempool history epoch) = foldr add setChain transactions
 
-- * Exercise i) 
fold :: (a -> b -> b) -> b -> SetChain a -> b 
fold f base (SC mempool history epoch) = foldr (flip (S.fold f)) base (D.values history) 
        
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
