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
isEmpty (SC mempool history _) = S.isEmpty mempool && D.isEmpty history 

-- * Exercise c) 
getEpoch :: (Eq a) => a -> SetChain a -> Integer 
getEpoch x (SC _ history _) = buscar x (D.keys history)
  where
    buscar _ [] = -1
    buscar x (k:ks) = 
      case D.valueOf k history of
        Just set -> if S.isElem x set then k else buscar x ks
        Nothing -> error "Error"
 
-- * Exercise d) 
size :: SetChain a -> Int 
size (SC _ history _) = contar (D.keys history)
  where
    contar [] = 0
    contar (k:ks) =
      case D.valueOf k history of
        Just set -> S.size set + contar ks
        Nothing  -> contar ks
 
-- * Exercise e) 
pendingTransactions :: SetChain a -> Bool 
pendingTransactions (SC mempool _ _) = not(S.isEmpty mempool)
 
-- * Exercise f) 
add :: (Eq a) => a -> SetChain a -> SetChain a 
add x (SC mempool history epoch) = SC (S.insert x mempool) history epoch
 
-- * Exercise g) 
validate :: (Eq a) => SetChain a -> SetChain a 
validate (SC mempool history epoch)
  | pendingTransactions (SC mempool history epoch) == False = (SC mempool history epoch)
  | existeTransaccionHistorial mempool (D.keys history) == True = error "transaction already validated"
  | otherwise = SC S.empty (D.insert epoch mempool history) (epoch+1)
  where
    existeTransaccionHistorial s ks = S.fold (\x acc -> acc || existeTransaccion x ks) False s

    existeTransaccion _ [] = False
    existeTransaccion x (k:ks) =
      case D.valueOf k history of
        Just set -> S.isElem x set || existeTransaccion x ks
        Nothing  -> existeTransaccion x ks
 
-- * Exercise h) 
addAll :: (Eq a) => [a] -> SetChain a -> SetChain a 
addAll xs (SC mempool history epoch) = SC (insertar xs mempool) history epoch
  where
    insertar [] mempool = mempool 
    insertar (x:xs) mempool = insertar xs (S.insert x mempool)
 
-- * Exercise i) 
fold :: (a -> b -> b) -> b -> SetChain a -> b 
fold f z (SC _ history _) = plegarBloques (D.keys history) z
  where
    plegarBloques [] acc = acc
    plegarBloques (k:ks) acc = 
      case D.valueOf k history of
        Just bloque -> let acc' = S.fold f acc bloque
                       in plegarBloques ks acc'
        Nothing     -> plegarBloques ks acc
        
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