module Main where

import GameCard
import Player
import Game
import System.IO
import Data.Char (digitToInt, isSpace)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)

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
     putStrLn $ (++ " wins!") $ show $ name (getPlayer (cplayer ref) (gamecard ref))
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
  let gstate = gamecard gsraw
  let pn = cplayer gsraw
  print gstate
  putStrLn "\nEnter the dart you hit, a single space, and the number of markings.\
            \\nFor example: 15 3 ,15 2, 15 1 or 0 for nothing.\n"
  ds <- getLine
  let marked = mark (getPlayer pn gstate) (getPlayer 2 gstate) ds
  writeIORef gst gsraw {gamecard = (setPlayer pn gstate marked)
                       ,cplayer = (if ctime gsraw == 0 then increment pn else pn)}
  return gst

getValidInput :: Int -> IO String
getValidInput n = helper n ""
    where helper 1 s = return s
          helper 0 s = do
            x <- getLine
            if any isSpace x
             then let (sub, end) = break isSpace x
                  in if read sub > 21 || read end > 3
                     then putStrLn "Invalid input. Please try again." >> helper 0 s
                     else helper 1 x
             else if read x == 0 
                  then helper 1 x
                  else putStrLn "Invalid input. Please try again." >> helper 0 s