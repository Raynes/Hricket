module Main where

import GameCard
import Player
import Game
import System.IO
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
  putStr "Welcome to Hricket. The Haskell cricket scoring application.\n\n\
            \Please enter player 1's name: "
  p1 <- getLine
  putStr "\nPlease enter player 2's name: "
  p2 <- getLine
  newIORef $ newGame p1 p2

dartPrompt gst = do
  gsraw' <- readIORef gst
  let gsraw = gsraw' {ctime = (incrementT (ctime gsraw'))}
  let gstate = gamecard gsraw
  let pn = cplayer gsraw
  print gstate
  putStrLn "\nEnter the dart you hit, a single space, and the number of markings.\
            \\nFor example: 15 Three or 15 One or 15 Two or 0 None for nothing.\n"
  ds <- getLine
  let marked = mark (getPlayer pn gstate) (getPlayer 2 gstate) ds
  writeIORef gst gsraw {gamecard = (setPlayer pn gstate marked)
                       ,cplayer = (if ctime gsraw == 0 then increment pn else pn)}
  return gst