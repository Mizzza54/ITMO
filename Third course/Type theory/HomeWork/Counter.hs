{-# LANGUAGE RankNTypes #-}

module TT.Counter where
  
data AbstractCounter = AC (forall b . (forall a . ( a          -- Начальный счетчик, значение 0
                                                , a -> a       -- Инкремент
                                                , a -> a       -- Декремент
                                                , a -> Bool    -- Проверка на ноль
                                                , a -> Integer -- Getter
                                                , a -> String
                                                ) -> b) -> b)

sendGet :: AbstractCounter -> Integer
sendGet counter =
  case counter of
    AC r -> r x where
      x (state', _, _, _, get', _) = get' state'
      
sendToString counter =
               case counter of
                 AC r -> r x where
                   x (state', _, _, _, get', toString') = toString' state'

sendInc :: AbstractCounter -> AbstractCounter
sendInc counter =
  case counter of
    AC r -> r x where
      x (state', inc', dec', isZero', get', toString') =
        AC (\t -> t (inc' state', inc', dec', isZero', get', toString'))

sendDec :: AbstractCounter -> AbstractCounter
sendDec counter =
  case counter of
    AC r -> r x where
      x (state', inc', dec', isZero', get', toString') =
        AC (\t -> t (dec' state', inc', dec', isZero', get', toString'))

sendIsZero :: AbstractCounter -> Bool
sendIsZero counter =
  case counter of
    AC r -> r x where
      x (state', _, _, isZero', _, _) =
        let v = isZero' state' in v

intCounter :: AbstractCounter
intCounter = AC (\t -> t (intInit, intInc, intDec, intIsZero, intGet, intToString))

listCounter :: AbstractCounter
listCounter = AC (\t -> t (listInit, listInc, listDec, listIsZero, listGet, listToString))

pairCounter :: AbstractCounter
pairCounter = AC (\t -> t (pairInit, pairInc, pairDec, pairIsZero, pairGet, pairToString))

intInit :: Integer
intInit = 0
intInc :: Integer -> Integer
intInc = (+1)
intDec :: Integer -> Integer
intDec = subtract 1
intIsZero :: Integer -> Bool
intIsZero = (== 0)
intGet :: Integer -> Integer
intGet = id
intToString :: Integer -> String
intToString = show 

listInit :: [[Integer]]
listInit = []
listInc :: [[Integer]] -> [[Integer]]
listInc x = [] : x
listDec :: [[Integer]] -> [[Integer]]
listDec = drop 1
listIsZero :: [[Integer]] -> Bool
listIsZero [] = True
listIsZero _ = False
listGet :: [[Integer]] -> Integer
listGet = toInteger . length
listToString :: [[Integer]] -> String
listToString = show 

pairInit :: (Integer, Integer)
pairInit = (0, 0)
pairInc :: (Integer, Integer) -> (Integer, Integer)
pairInc pair = (fst pair, snd pair + 1)
pairDec :: (Integer, Integer) -> (Integer, Integer)
pairDec pair = (fst pair + 1, snd pair)
pairIsZero :: (Integer, Integer) -> Bool
pairIsZero = uncurry (==)
pairGet :: (Integer, Integer) -> Integer
pairGet pair = snd pair - fst pair
pairToString :: (Integer, Integer) -> String
pairToString = show

listOfCounters :: [AbstractCounter]
listOfCounters = [intCounter, listCounter, pairCounter]


messageSequence :: [[Char]]
messageSequence = [ "sendIsZero"
                  , "sendInc"
                  , "sendInc"
                  , "sendDec"
                  , "sendInc"
                  , "sendIsZero"
                  , "sendDec"
                  , "sendIsZero"
                  , "sendDec"
                  , "sendIsZero"
                  , "sendDec"
                  , "sendIsZero"
                  , "sendInc"
                  , "sendIsZero"
                  , "end"]
                  
executeMessage :: String -> AbstractCounter -> (AbstractCounter, String)                   
executeMessage "sendInc" counter      = (sendInc counter, "(+1)")
executeMessage "sendDec" counter      = (sendDec counter, "(-1)")
executeMessage "sendIsZero" counter   = (counter, show $ sendIsZero counter)
executeMessage "sendGet" counter      = (counter, show $ sendGet counter)
executeMessage "sendToString" counter = (counter, sendToString counter)
executeMessage _ _                    = error "Bad message"



test :: [[Char]] -> AbstractCounter -> [Char]
test ["end"] _         = "end."
test message counter = 
  let result = executeMessage (head message) counter in
    snd (executeMessage "sendGet" counter) 
    ++ " = "
    ++ snd (executeMessage "sendToString" counter) 
    ++ ", " 
    ++ snd result 
    ++ ", " 
    ++ test (tail message) (fst result)


main :: IO ()
main =
   print [test messageSequence counter | counter <- listOfCounters]

--test messageSequence intCounter
--test messageSequence listCounter
--test messageSequence pairCounter
