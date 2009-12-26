module Card where

import Mark
import qualified Data.Map as M

type Score = Int

data Card = Card (M.Map Int Mark) Score

instance Show Card where
  show (Card m s) = foldl step "" (M.toList m) ++ "Score: " ++ show s
    where step xs (n,mark) = xs ++ (show n ++ ": ") ++ show mark ++ "\n"

createCard :: Card
createCard = Card (M.fromList (zip [15..21] (replicate 7 None))) 0

getMap :: Card -> M.Map Int Mark
getMap (Card m _) = m

setMap :: Card -> M.Map Int Mark -> Card
setMap (Card _ s) m = Card m s 

getScore :: Card -> Score
getScore (Card _ s) = s

setScore :: Card -> Score -> Card
setScore (Card m _) = Card m

