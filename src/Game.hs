module Game (isGameOver
            ,mark
            ,dartString) where

import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Player
import Mark
import Card

isGameOver :: Player -> Player -> Bool
isGameOver p1 p2 = ((||) `on` (== replicate 7 Three)) play1 play2
    where (play1,play2) = 
              (map snd (M.toList $ getCMap p1),map snd (M.toList $ getCMap p2))

mark :: Player -> Player -> String -> Player
mark p1 p2 = setMarks p1 p2 . dartString

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

multNum :: Mark -> (Int -> Int)
multNum None  = id
multNum One   = id
multNum Two   = (*2)
multNum Three = (*3)