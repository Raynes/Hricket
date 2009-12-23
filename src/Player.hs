module Player where

import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad (ap)
import Data.Function (on)

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

createCard :: Card
createCard = Card (M.fromList (zip [15..21] (replicate 5 None))) 0

getMap :: Card -> M.Map Int Mark
getMap (Card m _) = m

setMap :: Card -> M.Map Int Mark -> Card
setMap (Card _ s) m = Card m s 

getScore :: Card -> Score
getScore (Card _ s) = s

setScore :: Card -> Score -> Card
setScore (Card m _) = Card m

data Player = Player String Card

instance Show Player where
  show (Player s c) = "Name: " ++ s ++ "\n" ++ show c

createPlayer :: String -> Player
createPlayer = (flip Player) createCard

name :: Player -> String
name (Player s _) = s

card :: Player -> Card
card (Player _ c) = c

setCard :: Player -> Card -> Player
setCard (Player s _) = Player s

data ScoreCard = ScoreCard Player Player

instance Show ScoreCard where
  show (ScoreCard c c2) =
    intercalate "\n" $ zipWith (++) 
    (map (\x -> x ++ replicate (20 - length x) ' ') $ lines $ show c)
    (lines $ show c2)
    
  
------------------------------------------------------
-- Code to work with the types. ----------------------
------------------------------------------------------


setMarks :: Player -> Player -> [(Int, Mark)] -> Player
setMarks p p2 darts = setCard p $ setMap pc1 $ getMap $ foldr step pc1 darts
    where (pc1,pc2) = (card p, getMap (card p2))
          step (n,m) ys = let (Just p2cur) = M.lookup n pc2
                              (Just p1cur) = M.lookup n $ getMap ys
                          in if ((&&) `on` (== Closed)) p2cur p1cur || n < 15
                             then ys
                             else if p1cur < Closed
                                  then setMap ys $ M.update (const (Just (p1cur + m))) n (getMap ys)
                                  else setScore ys $ m `multNum` getScore ys

multNum :: Mark -> (Int -> Int)
multNum None   = id
multNum One    = id
multNum Two    = (*2)
multNum Closed = (*3)