module GameCard (createNew, GameCard, setPlayer, getPlayer) where

import Player
import Data.List

data GameCard = GameCard Player Player

instance Show GameCard where
  show (GameCard c c2) =
    intercalate "\n" $ zipWith (++) 
    (map (\x -> x ++ replicate (20 - length x) ' ') $ lines $ show c)
    (lines $ show c2)

setPlayer :: Int -> GameCard -> Player -> GameCard
setPlayer n gcard p = case n of
                        1 -> setPlayer1 gcard p
                        2 -> setPlayer2 gcard p

setPlayer1 :: GameCard -> Player -> GameCard
setPlayer1 (GameCard _ p2) = flip GameCard p2

setPlayer2 :: GameCard -> Player -> GameCard
setPlayer2 (GameCard p1 _) = GameCard p1

getPlayer :: Int -> GameCard -> Player
getPlayer n gcard = case n of
                      1 -> getPlayer1 gcard
                      2 -> getPlayer2 gcard

getPlayer1 :: GameCard -> Player
getPlayer1 (GameCard p _) = p

getPlayer2 :: GameCard -> Player
getPlayer2 (GameCard _ p) = p

createNew :: String -> String -> GameCard
createNew p1name p2name = GameCard (createPlayer p1name) (createPlayer p2name)