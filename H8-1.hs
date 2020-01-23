------------------
-- Aufgabe H8-1 --
------------------

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show
aBST :: Num a => Tree a
aBST  = Node (Node  ( Node (Leaf) 1 (Leaf)) 5 (Node (Leaf) 9 (Leaf))) 12 (Node (Node (Leaf) 15 (Leaf)) 19 (Node (Node (Leaf) 21 (Leaf)) 28 (Node (Leaf) 42 (Leaf))))

toAscList :: Ord a => Tree a -> [a]
toAscList (Leaf)        = []
toAscList (Node l x r)  = toAscList l ++ [x] ++ toAscList r 

lookupRange :: Ord a => a -> a -> Tree a -> Bool
lookupRange low high Leaf         = False
lookupRange low high (Node l x r) = ((>=low) x && (<=high) x) || lookupRange low high l || lookupRange low high r