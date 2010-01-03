module Main where

import GameCard
import Player
import Game
import Card
import System.IO
import Data.Char (isDigit, isSpace)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (liftM2)

data GState = GState {gamecard :: GameCard, cplayer :: Int
                     ,ctime :: Int}
              deriving (Show)

newGame :: String -> String -> GState
newGame p1n p2n = GState (createNew p1n p2n) 1 0

increment :: Int -> Int
increment n = if n == 2 then 1 else 2

incrementT n = if n < 2 then n+1 else 0

mainLoop :: IO (IORef GState) -> IO ()
mainLoop dp = do
  res <- dp
  gstate <- readIORef res
  if isGameOver (getPlayer 1 (gamecard gstate)) (getPlayer 2 (gamecard gstate))
   then do 
     print (gamecard gstate)
     ref <- readIORef res
     let p1 = getScore (card (getPlayer (cplayer ref) (gamecard ref)))
         p2 = getScore (card (getPlayer (increment (cplayer ref)) (gamecard ref)))
         winner = if p1 > p2 then cplayer ref else increment (cplayer ref)
     putStrLn $ (++ " wins!") $ show $ name (getPlayer winner (gamecard ref))
    else mainLoop $ dartPrompt res

main :: IO ()
main = do
  prompted <- prompt
  mainLoop $ dartPrompt prompted

prompt :: IO (IORef GState)
prompt = do
  putStrLn "Welcome to Hricket. The Haskell cricket scoring application.\n\n\
         \Please enter player 1's name: "
  p1 <- getLine
  putStrLn "\nPlease enter player 2's name: "
  p2 <- getLine
  putStr "\n"
  newIORef $ newGame p1 p2

dartPrompt gst = do
  gsraw' <- readIORef gst
  let gsraw = gsraw' {ctime = (incrementT (ctime gsraw'))}
      gstate = gamecard gsraw
      pn = cplayer gsraw
      play = getPlayer pn gstate
  print gstate
  putStr $ "\nEnter the dart you hit, a single space, and the number of markings.\
           \\nFor example: 15 3 ,15 2, 15 1 or 0 for nothing.\n" ++ name play ++
           ": "
  hFlush stdout
  ds <- getValidInput play
  let marked = mark (getPlayer pn gstate) (getPlayer (increment pn) gstate) ds
  writeIORef gst gsraw {gamecard = (setPlayer pn gstate marked)
                       ,cplayer = (if ctime gsraw == 0 then increment pn else pn)}
  return gst

getValidInput :: Player -> IO String
getValidInput player = helper 0 ""
    where helper 1 s = return s
          helper 0 s = do
            x <- getLine
            case checkInput x player of
              Right y -> helper 1 y
              Left  y -> putStr y >> hFlush stdout >> helper 0 ""

checkInput x player
  | not $ noLetters x = Left str
  | any isSpace x && not (isSpace (last x)) =
    let (sub, end) = break isSpace x
        y = read sub
        l = read end
    in
      if y > 20 && y < 25 || y < 1 || l > 3 || l < 1 
      then Left str
      else Right x
  | read x == 0 = Right x
  | otherwise = Left str
  where noLetters = all (liftM2 (||) isDigit isSpace)
        str = "Invalid input. Please try again.\n" ++ name player ++ ": " 