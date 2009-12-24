module Player (Mark(..), Player, createPlayer, mark, isGameOver)  where

import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad (ap)
import Data.Function (on)
import Data.Maybe

type Score = Int

-- A basic numeric type to represent
-- the number of marks a number has on it.
data Mark = None | One | Two | Three
            deriving (Show,Eq,Ord,Read)

instance Num Mark where
  (+) None x   = x
  (+) x None   = x
  (+) One One  = Two
  (+) Three _ = Three
  (+) _ Three = Three
  (+) One Two  = Three
  (+) Two One  = Three
  (+) Two Two  = Three
  
  fromInteger 0 = None
  fromInteger 1 = One
  fromInteger 2 = Two
  fromInteger 3 = Three

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
  
------------------------------------------------------
-- The beef. -----------------------------------------
------------------------------------------------------

isGameOver :: Player -> Player -> Bool
isGameOver p1 p2 = ((&&) `on` (== replicate 7 Three)) play1 play2
    where (play1,play2) = 
              (map snd (M.toList $ getCMap p1),map snd (M.toList $ getCMap p2))

mark :: Player -> Player -> String -> (Player, Bool)
mark p1 p2 darts = let np = setMarks p1 p2 (dartString darts)
                   in (np, isGameOver np p2)

dartString :: String -> (Int, Mark)
dartString s = let (x:y:[]) = words s 
               in (read x :: Int, read y :: Mark) 

setMarks :: Player -> Player -> (Int, Mark) -> Player
setMarks p p2 (n,m) = setCard p $ let p2cur = fromMaybe 0 $ M.lookup n pc2
                                      p1cur = fromMaybe 0 $ M.lookup n $ getMap pc1
                                  in if ((&&) `on` (== Three)) p2cur p1cur || n < 15
                                     then pc1
                                     else if p1cur < Three
                                          then setMap pc1 $ M.update (const (Just (p1cur + m))) n (getMap pc1)
                                          else setScore pc1 $ m `multNum` n + getScore pc1
    where (pc1,pc2) = (card p, getCMap p2)


getCMap :: Player -> M.Map Int Mark
getCMap = getMap . card

multNum :: Mark -> (Int -> Int)
multNum None  = id
multNum One   = id
multNum Two   = (*2)
multNum Three = (*3)