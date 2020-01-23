{-# LANGUAGE InstanceSigs #-} 
-- 10. Übung Programmierung und Modellierung, SoSe19, TCS, LMU  München 
-- H10-1 IO mit Akkumulator
import System.IO          -- um die Pufferung abzuschalten
import Data.Char as Char  -- für isSpace 
-- ein einfacher Aufzählungstyp
data Op = Add | Subtract | Multiply | Divide
  deriving (Eq,Enum)

-- Wandlung nach String 
instance Show Op where   
  show Add      = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide   = "/"  
  
-- Wandlung von String 
instance Read Op where 
  readsPrec :: Int -> String -> [(Op,String)] -- Precedence, String, mögliche Parsing Alternativen   
  readsPrec _ ('+':r) = [(Add,r)]
  readsPrec _ ('-':r) = [(Subtract,r)]
  readsPrec _ ('*':r) = [(Multiply,r)]
  readsPrec _ ('/':r) = [(Divide,r)]
  readsPrec _ other   = []
  -- Anmerkung: das Präzendenz-Argument können wir ignorieren, da wir nur ein
  -- einzelnes Symbol erkennen müssen und keine zusammengesetzten 
  -- Ausdrücke mit impliziter Klammerung (im Gegensatz zu Folien 7.30ff.) 
  
-- Die Funktion read aus der Standardbibliothek 
--   read :: Read a => a -> String   
-- is leider eine partielle Funktion, da Parsing fehlschlagen kann. 
-- Wir schreiben uns eine äquivalente totale Funktion:
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of 
                [(x,"")] -> Just x  -- only accept single parse 
                _        -> Nothing

-- Hier geht es los mit dem eigentlichen Programm:
main :: IO () 
main = do 
  hSetBuffering stdout NoBuffering  -- sorgt dafür, das jedes Zeichen sofort ausgegeben wird (Folie 9.31)
  putStrLn "Willkommen zum Taschenrechner!"
  putStrLn "Pro Zeile: Zahl oder +, -, *, / eingeben"
  putStrLn "Zum Beenden eine leere Zeile eingeben"
  (total,_) <- hilf (0,Nothing) -- Initialisierung des Akkumulators 
  putStrLn $ "Endergebnis: " ++ show total 


hilf :: (Double, Maybe Op) -> IO (Double, Maybe Op) 
hilf (total,op) = do
  putStr $ show total ++ " > "   
  input <- dropWhile Char.isSpace <$> getLine 
  case (maybeRead input, maybeRead input) of 
    (Just val,             _ ) -> hilf (perform op total val, Nothing) 
    (Nothing , newOp@(Just _)) -> hilf (total, newOp)
    _ -> if null input 
            then do                                                                           
                return (total, op)  
            else do            
                return (total + 1, Nothing) 
                putStrLn "Eingabe nicht akzeptiert!"
                hilf (total, op)                    
  where 
    perform Nothing         = flip const
    perform (Just Add)      = (+) 
    perform (Just Subtract) = (-) 
    perform (Just Multiply) = (*) 
    perform (Just Divide)   = (/) 

    
