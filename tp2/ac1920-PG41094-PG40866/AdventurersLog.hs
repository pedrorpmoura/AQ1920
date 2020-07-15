{-# LANGUAGE FlexibleInstances #-}

module AdventurersLog where

import DurationMonad
import Cp

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
crossIsValid l s = length l <= 2
                && all (\a -> s (Left a) == s (Right ())) l  

cross :: [Adventurer] -> State -> LogListDur Movement State
cross l s = if crossIsValid l s
                then LLD [ ([m], Duration (maxT, s')) ]
                else LLD []
            where maxT = maximum (map getTimeAdv l)
                  s' = mChangeState ((Right ()) : map Left l) s
                  m = Mov { from = s (Right ()),
                            to = s' (Right ()),
                            adventurers = l,
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
leq17 = any (\(_, Duration (t, s)) -> t <= 17 && s == const True) . remLLD $ exec 5 gInit

l17 :: Bool
l17 = any (\(_, Duration (t, s)) -> t < 17 && s == const True) . remLLD $ exec 5 gInit

solution :: ([Movement], Duration State)
solution = head . dropWhile (\(_, Duration (t, s)) -> t > 17 || s /= const True) . remLLD $ exec 5 gInit


----
data LogListDur t a = LLD [([t], Duration a)]
    deriving (Show, Eq)

instance Monad (LogListDur t) where
    return = LLD . singl . split nil return
    l >>= f = LLD . concatMap (\(l', Duration (t,x)) 
                -> map ((l' ++) >< (wait t)) (remLLD (f x))) . remLLD $ l    
    --l >>= f = LLD $ do
    --            (l1, Duration (t, x)) <- remLLD l
    --            let u = remLLD (f x) in 
    --                map (\(l2, d) -> (l1 ++ l2, wait t d)) u


remLLD :: LogListDur t a -> [([t], Duration a)]
remLLD (LLD l) = l 


instance Functor (LogListDur t) where
    fmap f = LLD . map f' . remLLD
        where f' = \(l, d) -> (l, fmap f d)


instance Applicative (LogListDur t) where
    pure = LLD . singl . split nil pure
    fs <*> l = LLD [ (l2 ++ l1, df <*> d ) | (l1, df) <- remLLD fs, (l2, d) <- remLLD l ]
                
manyChoiceLLD :: [LogListDur t a] -> LogListDur t a
manyChoiceLLD = LLD . concatMap remLLD 

