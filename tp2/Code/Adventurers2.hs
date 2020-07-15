{-# LANGUAGE FlexibleInstances #-}

module Adventurers2 where

import DurationMonad

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv P1  = 1
getTimeAdv P2  = 2
getTimeAdv P5  = 5
getTimeAdv P10 = 10


advList :: [Adventurer]
advList = [P1,P2,P5,P10]

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1)  == s2 (Left P1),
                    s1 (Left P2)  == s2 (Left P2),
                    s1 (Left P5)  == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
gInit :: State
gInit = const False



-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)


ex1 :: State
ex1 = changeState (Right ()) gInit

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os


data Movement = Mov {
    from :: Bool,
    to :: Bool,
    adventurers :: [Adventurer],
    time :: Int
}

instance Show Movement where
    show m = if from m == False && to m == True
                then "False ---" ++ show (adventurers m) ++ " t=" ++ show (time m) ++ "--> True"
                else "False <--" ++ show (adventurers m) ++ " t=" ++ show (time m) ++ "--- True"

crossIsValid :: [Adventurer] -> State -> Bool
crossIsValid ps s = length ps <= 2
                 && all (\p -> s (Left p) == s (Right ())) ps

cross :: [Adventurer] -> State -> LogListDur Movement State
cross ps s = if crossIsValid ps s
                then LLD [ Duration (maxT, Log [([m], s')])]
                else LLD []
            where maxT = maximum (map getTimeAdv ps)
                  s' = mChangeState ((Right ()) : map Left ps) s
                  m = Mov { from = s (Right ()),
                            to = s' (Right ()),
                            adventurers = ps,
                            time = maxT }
                

oneCrossing :: State -> LogListDur Movement State
oneCrossing s = manyChoiceLLD [ cross [p] s | p <- advList ]

twoCrossing :: State -> LogListDur Movement State
twoCrossing s = manyChoiceLLD [ cross p s | p <- possiblePairs ] -- more efficient
         where possiblePairs = [[P1,P2], [P1,P5], [P1,P10], [P2,P5], [P2, P10], [P5,P10]]

allValidPlays :: State -> LogListDur Movement State
allValidPlays s = manyChoiceLLD [ oneCrossing s, twoCrossing s ]


exec :: Int -> State -> LogListDur Movement State
exec 0 s = return s
exec n s = allValidPlays s >>= exec (n-1)

leq17 :: Bool
leq17 = any (\(Duration (d, Log [(_, s)])) -> d <= 17 && s == const True) (remLLD (exec 5 gInit))

solution :: Duration (LogList Movement State)
solution = head $ filter (\(Duration (d, Log [(_, s)])) -> d <= 17 && s == const True) (remLLD (exec 5 gInit))


------- LOG LIST --------
data LogList t a = Log [([t], a)]
    deriving (Show)

remLog :: LogList t a -> [([t], a)]
remLog (Log l) = l

ex :: LogList Char Int
ex = Log [("Hello", 1), ("Ola", 2)]

instance Functor (LogList t) where
    fmap f = Log . map f' . remLog
        where f' = \(s, x) -> (s, f x)


instance Applicative (LogList t) where
    pure x = Log [([],x)]
    fs <*> l = Log [ (s1 ++ s2, f x) | (s1, f) <- remLog fs, (s2, x) <- remLog l ]


instance Monad (LogList t) where
    return = pure
    l >>= k = Log $ do
                (s, x) <- remLog l
                map (\(m, y) -> (s ++ m, y)) . remLog $ k x

mwrite :: [t] -> LogList t a -> LogList t a
mwrite msg = Log . map (\(m, y) -> (msg ++ m, y)) . remLog



----
data LogListDur t a = LLD [Duration (LogList t a)]
    deriving (Show)

remLLD :: LogListDur t a -> [Duration (LogList t a)]
remLLD (LLD l) = l

exLLD :: LogListDur Char State
exLLD = LLD [Duration (0, log1), Duration (2, log2)]
    where
        log1 = Log [("ZERO", gInit), ("ONE", mChangeState [Left P1, Right ()] gInit)]
        log2 = Log [("TWO", mChangeState [Left P2, Right ()] gInit)]

test :: LogListDur Char (State -> Bool)
test = LLD [Duration (1, Log [("WHERE IS LANTERN? ", \s -> s (Right ()))])]

instance Functor (LogListDur t) where
    fmap f = LLD . map f' . remLLD
        where f' = fmap (fmap f)


instance Applicative (LogListDur t) where
    pure x = LLD [ (pure . pure) x ]
    fs <*> l = LLD [ fmap (<*>) f <*> x | f <- remLLD fs, x <- remLLD l ]


instance Monad (LogListDur t) where
    return = pure
    l >>= k = LLD $ do
                (Duration (d, Log x)) <- remLLD l
                let u = map (\(t, s) -> (t, remLLD (k s))) x in
                    concatMap (\(t1, l1) -> map (\(Duration (d', l')) -> Duration (d' + d, mwrite t1 l')) l1) u


manyChoiceLLD :: [LogListDur t a] -> LogListDur t a
manyChoiceLLD = LLD . concatMap remLLD 
