module Player where

import qualified Data.Map as M
import Data.List (intercalate)

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

getScore :: Card -> Score
getScore (Card _ s) = s

setScore :: Card -> Score -> Card
setScore (Card m _) s = Card m s

data Player = Player String Card
              deriving (Show)

name :: Player -> String
name (Player s _) = s

card :: Player -> Card
card (Player _ c) = c

data ScoreCard = ScoreCard Player Player

instance Show ScoreCard where
  show (ScoreCard (Player _ c) (Player _ c2)) =
    intercalate "\n" $
    zipWith (++) (map (\x -> x ++ replicate (12 - length x) ' ') $ lines $ show c)
            (lines $ show c2)
    
  
------------------------------------------------------
-- Code to work with the types. ----------------------
------------------------------------------------------

setMarks :: Player -> Player -> [(Int, Mark)] -> Card
setMarks p p2 darts = undefined -- TODO