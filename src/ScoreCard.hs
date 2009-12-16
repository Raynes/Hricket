module ScoreCard where

import qualified Data.Map as M

type Score = Int

data Mark = None | One | Two | Closed
            deriving (Show,Eq)

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
            deriving (Show)

getMap :: Card -> M.Map Int Mark
getMap (Card m _) = m

getScore :: Card -> Score
getScore (Card _ s) = s

setScore :: Card -> Score -> Card
setScore (Card m _) s = Card m s

{- Incomplete
setMarks :: Card -> [(Int, Mark)] -> Card
setMarks c alist = foldr -}