-- Vorlage fuer H6-2

data HuffmanTree = Leaf Char
                 | Node HuffmanTree HuffmanTree
                   deriving Show

german :: HuffmanTree
german = Node (Node (Node (Leaf 'n') (Node (Leaf 'd') (Node (Node (Leaf 'k') (Leaf 'f')) (Leaf 'o')))) (Node (Node (Node (Leaf 'c') (Leaf 'g')) (Leaf 't')) (Node (Leaf 's') (Node (Node (Node (Leaf 'v') (Node (Node (Node (Node (Leaf 'q') (Leaf 'y')) (Leaf 'x')) (Leaf 'j')) (Leaf 'p'))) (Leaf 'b')) (Node (Node (Leaf 'z') (Leaf 'w')) (Leaf 'm')))))) (Node (Node (Leaf ' ') (Node (Leaf 'r') (Node (Leaf 'u') (Leaf 'l')))) (Node (Leaf 'e') (Node (Leaf 'i') (Node (Leaf 'h') (Leaf 'a')))))


codes :: HuffmanTree -> [(Char, String)]
codes tree = codesAux [] tree where
    codesAux :: [Char] -> HuffmanTree -> [(Char, [Char])]
    codesAux acc (Leaf char) = [(char,acc)] 
    codesAux acc (Node left right) = codesAux (acc ++ "0") left ++ codesAux (acc ++ "1") right

encode :: HuffmanTree -> String -> String
encode tree text =  encodeAux [] (codes tree) text where
    encodeAux :: [Char] -> [(Char, [Char])] -> [Char] -> [Char]
    encodeAux acc codedTree []          = acc
    encodeAux acc codedTree (head:rest) = encodeAux (acc ++ (case lookup head codedTree of   
                                                                         Nothing -> [] 
                                                                         Just x  -> x  )) codedTree rest 


   