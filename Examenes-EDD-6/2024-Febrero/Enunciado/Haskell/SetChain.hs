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
empty = undefined

-- * Exercise b) 
isEmpty :: SetChain a -> Bool
isEmpty = undefined

-- * Exercise c) 
getEpoch :: (Eq a) => a -> SetChain a -> Integer 
getEpoch = undefined
 
-- * Exercise d) 
size :: SetChain a -> Int 
size = undefined
 
-- * Exercise e) 
pendingTransactions :: SetChain a -> Bool 
pendingTransactions = undefined
 
-- * Exercise f) 
add :: (Eq a) => a -> SetChain a -> SetChain a 
add = undefined
 
-- * Exercise g) 
validate :: (Eq a) => SetChain a -> SetChain a 
validate = undefined
 
-- * Exercise h) 
addAll :: (Eq a) => [a] -> SetChain a -> SetChain a 
addAll = undefined
 
-- * Exercise i) 
fold :: (a -> b -> b) -> b -> SetChain a -> b 
fold = undefined
        
-- * Exercise j) 
toList :: SetChain a -> [a] 
toList = undefined

-- ------------------------------- 
-- DO NOT MODIFY THE CODE BELOW 
-- ------------------------------- 
 
instance (Show a) => Show (SetChain a) where 
  show (SC s h e) = concat ["SetChain(", show s, ", ", show h, ", ", show e, ")"] 
 
instance (Eq a) => Eq (SetChain a) where 
  (SC m1 h1 e1) == (SC m2 h2 e2) = m1 == m2 && h1 == h2 && e1 == e2
