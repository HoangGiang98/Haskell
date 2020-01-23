import Data.Maybe
import Data.Either
range :: Integer -> Integer -> [Integer] 
range x y | x > y = range y x
          | x==y =[x]
          | x < y = x:(range (x+1) y)

data AtMostTwo1 a = None | One a | Two a a  deriving (Eq, Show)
type AtMostTwo2 a = Either () (Either a (a, a))
type AtMostTwo3 a = Maybe (a, Maybe a)

convert1 :: AtMostTwo1 a -> AtMostTwo2 a
convert1 None = Left ()
convert1 (One a) = Right (Left a)
convert1 (Two x y)= Right (Right (x,y))
 
convert2 :: AtMostTwo2 a -> AtMostTwo3 a
convert2 (Left ()) = Nothing
convert2 (Right(Left a)) = Just (a, Nothing)
convert2 (Right (Right (x,y))) = Just (x, Just y)
 
convert3 :: AtMostTwo3 a -> AtMostTwo1 a
convert3 Nothing = None
convert3(Just(a,Nothing)) = One a
convert3(Just (x, Just y)) = Two x y

data Agent = Player String Double | AI Int | Spectator String
display :: Agent -> String
display (Player name _ ) = name
display (AI strength)    = "KI(" ++ show strength ++ ")"
display (Spectator name) = "{" ++ name ++ "}"

data HuffmanTree = Leaf Char
                 | Node HuffmanTree HuffmanTree
                   deriving Show
german :: HuffmanTree
german = Node (Node (Node (Leaf 'n') (Node (Leaf 'd') (Node (Node (Leaf 'k') (Leaf 'f')) (Leaf 'o')))) (Node (Node (Node (Leaf 'c') (Leaf 'g')) (Leaf 't')) (Node (Leaf 's') (Node (Node (Node (Leaf 'v') (Node (Node (Node (Node (Leaf 'q') (Leaf 'y')) (Leaf 'x')) (Leaf 'j')) (Leaf 'p'))) (Leaf 'b')) (Node (Node (Leaf 'z') (Leaf 'w')) (Leaf 'm')))))) (Node (Node (Leaf ' ') (Node (Leaf 'r') (Node (Leaf 'u') (Leaf 'l')))) (Node (Leaf 'e') (Node (Leaf 'i') (Node (Leaf 'h') (Leaf 'a')))))
                   
decode :: HuffmanTree -> String -> String
decode t x = decodeHelper t t x where
    decodeHelper :: HuffmanTree -> HuffmanTree -> String -> String
    decodeHelper t (Node l r) ('0':xs) = decodeHelper t l xs
    decodeHelper t (Node l r) ('1':xs) = decodeHelper t r xs
    decodeHelper t (Leaf a )  (x:xs)   = a:(decodeHelper t t (x:xs) )
    decodeHelper t (Leaf a )  []       = [a]
    decodeHelper t (Node l r) []       = [] 
    decodeHelper t (Node l r) (s:ss)   = s: decodeHelper t t ss