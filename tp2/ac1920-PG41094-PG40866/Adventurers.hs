{-# LANGUAGE FlexibleInstances #-}

module Adventurers where

import Cp
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
                               


{-- Only a maximum of 2 adventurers can cross at the same time,
    and they must be at the same side as the lantern --}
   
isCrossValid :: [Adventurer] -> State -> Bool
isCrossValid ps s = length ps <= 2 
                 && all (\p -> s (Left p) == s (Right ())) ps


cross :: [Adventurer] -> State -> ListDur State
cross ps s = if isCrossValid ps s 
               then LD [ Duration (maxT, mChangeState ((Right ()) : map Left ps) s) ]
               else LD []
         where maxT = maximum (map getTimeAdv ps)


oneCrossing :: State -> ListDur State
oneCrossing s = manyChoice [ cross [p] s | p <- advList ]

twoCrossing :: State -> ListDur State
-- twoCrossing s = manyChoice [ cross [p1,p2] s | p1 <- advList,  p2 <- advList, p1 /= p2 ]
twoCrossing s = manyChoice [ cross p s | p <- possiblePairs ] -- more efficient
         where possiblePairs = [[P1,P2], [P1,P5], [P1,P10], [P2,P5], [P2, P10], [P5,P10]]

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice [ oneCrossing s, twoCrossing s ]


{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = allValidPlays s >>= exec (n-1)


{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = any (\(Duration (d,s)) -> d <= 17 && s == const True) (remLD (exec 5 gInit))

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = any (\(Duration (d,s)) -> d < 17 && s == const True) (remLD (exec 5 gInit))

solution :: Duration State
solution = head $ filter (\(Duration (d,s)) -> d <= 17 && s == const True) (remLD (exec 5 gInit))

--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a]
   deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x


-- To implement
instance Functor ListDur where
   fmap f = LD . map f' . remLD
      where f' = \duration -> fmap f duration


-- To implement
instance Applicative ListDur where
   pure = LD . singl . pure
   fs <*> xs = LD [ f <*> x | f <- remLD fs, x <- remLD xs ]


-- To implement
instance Monad ListDur where
   return = LD . singl . return
   l >>= f = LD . concat . map (\(Duration (t,x)) 
               -> map (wait t) (remLD (f x))) . remLD $ l
   --l >>= f = LD $ do 
   --            Duration (d,x) <- remLD l
   --            map (wait d) (remLD (f x))
                        

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)