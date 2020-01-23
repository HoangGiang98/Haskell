import Prelude hiding (lex, const)
import Data.Char

------------------
-- Aufgabe H8-2 --
------------------

type Name = String
data Expr = Var String
          | Const Double
          | Plus Expr Expr
          | Times Expr Expr
  deriving (Eq)

-- Im Folgenden sind Show-Instanz und parse-Funktion zum leichteren
-- Testen definiert.
-- Die eigentliche Aufgabe beginnt auf Zeile 73.

instance Show Expr where
  show (Var x) = x
  show (Const i) = show i
  show (Plus e1 e2) = "("++ show e1 ++ "+" ++ show e2 ++")"
  show (Times e1 e2) = "("++ show e1 ++ "*" ++ show e2 ++")"

-- Code Folie 7.30
data Token = ID Name | CONST Double | LPAREN | RPAREN | PLUS | TIMES
  deriving Show 

-- Lexikalische Analyse
lex :: String -> [Token]
lex "" = []
lex (c:s) | isSpace c = lex s
lex ('(':s) = LPAREN : lex s
lex (')':s) = RPAREN : lex s
lex ('[':s) = LPAREN : lex s
lex (']':s) = RPAREN : lex s
lex ('+':s) = PLUS   : lex s
lex ('*':s) = TIMES  : lex s
lex (c:s) | isAlpha c = ID x : lex rest
  where (x, rest) = break (not . isAlphaNum) (c:s) 
lex s | [(i, rest)] <- reads s = CONST i : lex rest
lex s = error $ "Unbekanntes Zeichen: " <> s

-- Syntaxanalyse, Folie 7.31ff.
parseExpr   :: [Token] -> (Expr, [Token])
parseExpr l = case parseProd l of 
                (prod, PLUS:rest1) -> 
                  let (expr,rest2) = parseExpr rest1
                  in (Plus prod expr, rest2)
                (prod, rest) -> (prod, rest)

parseProd   :: [Token] -> (Expr, [Token])
parseProd l = case parseFactor l of 
                (factor, TIMES:rest1) -> 
                  let (expr, rest2) = parseProd rest1
                  in (Times factor expr, rest2)
                (factor, rest) -> (factor, rest)

parseFactor :: [Token] -> (Expr, [Token])
parseFactor ((ID x):rest) = (Var x, rest)
parseFactor ((CONST i):rest) = (Const i, rest)
parseFactor (LPAREN:rest) = case parseExpr rest of
                              (expr, RPAREN:rest2) -> (expr, rest2)
                              _ -> error "Schließende Klammer erwartet."
parseFactor _ = error "Syntaxfehler! Variable, Konstante oder öffnende Klammer erwartet."                             
  
parse :: String -> Expr
parse str = case parseExpr (lex str) of 
              (expr, []) -> expr 
              (_,bad) -> error $ "Unnötiger Ballast am Ende: " ++ show bad


-- TODO: Definieren Sie die Funktion diff
plus :: Expr -> Expr -> Expr 
plus (Const 0) a = a 
plus a (Const 0) = a
plus a  b        = Plus a b 
times :: Expr -> Expr -> Expr
times a (Const 1) = a 
times (Const 1) a = a 
times (Const 0) a = Const 0 
times a (Const 0) = Const 0
times a b         = Times a b
diff :: Name -> Expr -> Expr
diff x (Const _)      = Const 0
diff x (Plus  e1 e2)  = plus  (diff x e1 ) (diff x e2)
diff x (Times e1 e2)  = plus  (times (diff x e1) e2) (times e1 (diff x e2) )
diff x (Var y )
  | x == y    = Const 1
  | otherwise = Const 0
  
