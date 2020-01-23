-- Vorlage fuer H6-1

data List a = Nil | Cons a (List a) | RepeatCons Int a (List a)

-- TODO: Instanzen fuer Show (List a) und Eq (List a)
toBuiltin :: List a -> [a] 
toBuiltin Nil = []
toBuiltin (Cons x xs ) = x : toBuiltin xs    
toBuiltin (RepeatCons 0 x xs) = toBuiltin xs
toBuiltin (RepeatCons n x xs) = x : toBuiltin (RepeatCons (n-1) x xs )

instance Show a => Show (List a) where 
    show Nil = []   
    show xs = let (y:ys) = toBuiltin xs in "[" ++ showAux (y:ys) ++ "]"
       where
        showAux (y:[]) =  show y
        showAux (y:ys) =  show y ++ "; " ++ showAux ys 
instance Eq a => Eq (List a) where   
    xs == ys  = toBuiltin xs == toBuiltin ys    
 
   