module Player (Player, createPlayer, card, setCard, name, getCMap)  where

import qualified Data.Map as M
import Mark
import Card

type Score = Int

data Player = Player String Card

instance Show Player where
  show (Player s c) = "Name: " ++ s ++ "\n" ++ show c

createPlayer :: String -> Player
createPlayer = flip Player createCard

name :: Player -> String
name (Player s _) = s

card :: Player -> Card
card (Player _ c) = c

getCMap :: Player -> M.Map Int Mark
getCMap = getMap . card

setCard :: Player -> Card -> Player
setCard (Player s _) = Player s
