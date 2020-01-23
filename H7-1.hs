-- Vorlage fuer H7-1
--
-- Verwenden Sie in dieser Aufgabe 
-- WEDER REKURSION NOCH LIST-COMPREHENSIONS.
--
-- Erlaubt sind alle Funktionen der Prelude, 
-- insbesondere map, filter, zip, concat, reverse, foldr
-- 
-- Zur Definition von Listen können Sie Ausdrücke
-- der Form [n..m] verwenden. NICHT verwenden duerfen
-- Sie List-Comprehensions der Form [e | ...].
binaryToInteger :: [Bool] -> Integer
binaryToInteger xs = foldr (+) 0 
                        $ map (\(x,y) -> if (y == True) then (2 ^ x) else 0 ) 
                            $ zip [0.. (length xs) -1] $ reverse xs   

upto :: Eq a => (a -> Bool) -> [a] -> [a]
upto this xs = takeWhile (not.this) xs

addNoDuplicate :: Eq a => a -> [a] ->[a]
addNoDuplicate x []     =  [x]
addNoDuplicate x xs  
    | x == head xs   =  xs
    | otherwise      = x:xs

removeAdjacentDuplicates :: Eq a => [a] -> [a]
removeAdjacentDuplicates xs = foldr addNoDuplicate [] xs
                                                  


                                     