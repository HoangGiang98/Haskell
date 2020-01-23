class Monoid a => Log a where
  logMsg :: String -> a

fib :: Log a => Int -> (Int, a)
fib n =
  if n < 2 then (1, logMsg ("fib " ++ show n))
    else let (n1, log1) = fib (n-1)
             (n2, log2) = fib (n-2)
             y = n1 + n2
          in (y, log1 <> log2 <> logMsg ("fib " ++ show n))

data FullLog = FullLog [String] deriving Show
data ReverseLog = ReverseLog [String] deriving Show
data LastMsgLog = LastMsgLog (Maybe String) deriving Show
data CountLog = CountLog Int deriving Show
-- a)
instance Semigroup FullLog where
    FullLog a <> FullLog b = FullLog (a ++ b)
instance Monoid FullLog where 
    mempty = FullLog [] 
instance Log FullLog where
    logMsg xs = FullLog [xs]

instance Semigroup ReverseLog where
    ReverseLog a <> ReverseLog b = ReverseLog (b ++ a)    
instance Monoid ReverseLog where 
    mempty = ReverseLog []
instance Log ReverseLog where 
    logMsg xs = ReverseLog [xs]

instance Semigroup LastMsgLog where
    x                  <> LastMsgLog Nothing = x
    LastMsgLog a       <> LastMsgLog b       = LastMsgLog b
instance Monoid LastMsgLog where
    mempty = LastMsgLog (Nothing)
instance Log LastMsgLog where
    logMsg [] = LastMsgLog Nothing
    logMsg xs = LastMsgLog (Just xs )

instance Semigroup CountLog where
    CountLog a <> CountLog b = CountLog (a+b)
instance Monoid CountLog where
    mempty = CountLog 0
instance Log CountLog where 
    logMsg xs = CountLog 1 

-- b)
instance (Log a, Log b) => Log (a,b) where
    logMsg xs = (logMsg xs, logMsg xs)
      

