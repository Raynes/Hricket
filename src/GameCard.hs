module Game where

import Player
import Data.List

data GameCard = GameCard Player Player

instance Show GameCard where
  show (GameCard c c2) =
    intercalate "\n" $ zipWith (++) 
    (map (\x -> x ++ replicate (20 - length x) ' ') $ lines $ show c)
    (lines $ show c2)

getPlayer1 :: GameCard -> Player
getPlayer1 (GameCard p _) = p

getPlayer2 :: GameCard -> Player
getPlayer2 (GameCard _ p) = p

createNew :: String -> String -> GameCard
createNew p1name p2name = GameCard (createPlayer p1name) (createPlayer p2name)