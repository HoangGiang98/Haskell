import Data.Map (Map)
import qualified Data.Map as Map 
   hiding (fromList, fromListWith, fromListWithKey,
       fromAscList,fromAscListWith, fromAscListWithKey, fromDistinctAscList,
       fromDescList,fromDescListWith, fromDescListWithKey, fromDistinctDescList)
       -- Funktionen zur Erzeugung von Maps aus Listen sind nicht erlaubt
       -- und werden deshalb nicht importiert.

-- Die Deklaration aus dem Modul Data.Map sind hier mit Praefix "Map."
-- ansprechbar. Die Aufgabe kann mit folgenden Funktionen geloest werden.
--
-- Map.empty :: Map k a
-- Map.insert :: Ord k => k -> a -> Map k a -> Map k a
-- Map.delete :: Ord k => k -> Map k a -> Map k a
-- Map.lookup :: Ord k => k -> Map k a -> Maybe a

freq :: Ord a => [a] -> Map a Int
freq xs = foldl buildMap (Map.empty) xs 
  where buildMap m x
              | a == 0     = Map.insert x 1 m
              | otherwise  = Map.insert x (a+1) (Map.delete x m)
           where a = case (Map.lookup x m ) of 
                       Nothing -> 0
                       Just b  -> b          
                         
        
  
  

freqButBetter :: Ord a => [a] -> Map a Int 
freqButBetter = buildMap Map.empty 
  where
  buildMap m [] = m
  buildMap m (x:xs) = let value = Map.findWithDefault 0 x m 
                          value' = value+1
                          newMap = Map.insert x value' m
                      in value' `seq` newMap `seq` buildMap newMap xs