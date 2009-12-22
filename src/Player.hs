module Player where

import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad

type Score = Int

data Mark = None | One | Two | Closed
            deriving (Show,Eq,Ord)

data ScoreRes = S Int 
              | D Int 
              | T Int
              | N
                deriving (Show,Eq,Read)

instance Num Mark where
  (+) None x   = x
  (+) x None   = x
  (+) One One  = Two
  (+) Closed _ = Closed
  (+) _ Closed = Closed
  (+) One Two  = Closed
  (+) Two One  = Closed
  (+) Two Two  = Closed
  
  fromInteger 0 = None
  fromInteger 1 = One
  fromInteger 2 = Two
  fromInteger 3 = Closed

data Card = Card (M.Map Int Mark) Score

instance Show Card where
  show (Card m s) = foldl step "" (M.toList m) ++ "Score: " ++ show s
    where step xs (n,mark) = xs ++ (show n ++ ": ") ++ show mark ++ "\n"


getMap :: Card -> M.Map Int Mark
getMap (Card m _) = m

setMap :: Card -> M.Map Int Mark -> Card
setMap (Card _ s) m = Card m s 

getScore :: Card -> Score
getScore (Card _ s) = s

setScore :: Card -> Score -> Card
setScore (Card m _) = Card m

data Player = Player String Card
              deriving (Show)

name :: Player -> String
name (Player s _) = s

card :: Player -> Card
card (Player _ c) = c

setCard :: Player -> Card -> Player
setCard (Player s _) = Player s

data ScoreCard = ScoreCard Player Player

instance Show ScoreCard where
  show (ScoreCard (Player s c) (Player s2 c2)) =
    intercalate "\n" $  heading : zipWith (++) 
    (map (\x -> x ++ replicate (20 - length x) ' ') $ lines $ show c)
    (lines $ show c2)

    where heading = s ++ replicate (20 - length s) ' ' ++ s2
    
  
------------------------------------------------------
-- Code to work with the types. ----------------------
------------------------------------------------------

setMarks :: Player -> Player -> [(Int, Mark)] -> Player
setMarks p p2 darts = setCard p $ setMap pc1 $ foldr step pc1 darts
    where (pc1,pc2) = (card p, getMap (card p2))
          step (n,m) ys = let (Just p2cur) = M.lookup n pc2
                              (Just p1cur) = M.lookup n $ getMap ys
                          in if ((&&) `on` (== Closed)) p2cur p1cur
                             then ys
                             else if p1curr < Closed
                                  then setMap ys $ M.update (const (Just (p1curr + m))) n getMap ys
                                  else setScore ys $ getScore ys + 3