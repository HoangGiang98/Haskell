

data Token = Text String
           | Placeholder String
             deriving (Eq, Show)
takeText :: String -> (String,String)
takeText xs = loop xs []
        where loop [] acc  = (reverse acc, [])
              loop (y:ys) acc 
                 |  y /= '{'  = loop ys (y:acc)
                 |  otherwise = (reverse acc, y:ys)
        
takeHolder :: String -> (String,String)
takeHolder xs = loop xs []
        where loop (y:[]) acc = (reverse acc, [])
              loop (y:ys) acc 
                 |  y /= '}' = loop ys (y:acc)
                 |  otherwise = (reverse acc, ys)             
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('{':xs) = let (w, r) = takeHolder xs in  [ Placeholder w  ] ++ (tokenize r)
tokenize (xs) = let (w,r) = takeText xs in [ Text w ] ++ (tokenize r)
               

expectJust :: Maybe String -> String
expectJust (Just x) = x
expectJust Nothing  = []

replace :: [Token] -> [(String, String)] -> String
replace [] m = []
replace ((Text s):xs) m = s ++ replace xs m
replace ((Placeholder c):xs) m = expectJust( lookup c m ) ++ replace xs m
    