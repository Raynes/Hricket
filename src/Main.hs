module Main where

import GameCard
import Player
import System.IO
import Data.IORef
import Control.Monad

data GState = GState {gamecard :: GameCard, cplayer :: Int}
              deriving (Show)

newGame :: String -> String -> GState
newGame p1n p2n = GState (createNew p1n p2n) 1

increment :: Int -> Int
increment n = if n == 2 then 1 else 2

mainLoop :: IO ((IORef GState), Bool) -> IO ()
mainLoop dp = do
  (res,over) <- dp
  if over
   then readIORef res >>= putStrLn . ((++ " wins!") . show . cplayer)
   else mainLoop $ prompt >>= dartPrompt

main :: IO ()
main = do
  prompted <- prompt
  mainLoop $ dartPrompt prompted

prompt :: IO (IORef GState)
prompt = do
  putStrLn "Welcome to Hricket. The Haskell cricket scoring application.\n\n\
            \Please enter player 1's name: "
  p1 <- getLine
  putStrLn "Please enter player 2's name: "
  p2 <- getLine
  newIORef $ newGame p1 p2

dartPrompt :: IORef GState -> IO ((IORef GState),Bool)
dartPrompt gst = do
  gsraw <- readIORef gst
  let gstate = gamecard gsraw
  let pn = cplayer gsraw
  putStrLn $ show gstate
  putStrLn "Enter the dart you hit, a single space, and the number of markings.\
            \\nFor example: 15 Three or 15 One or 15 Two or 0 None for nothing."
  ds <- getLine
  let marked = mark (getPlayer pn gstate) (getPlayer 2 gstate) ds
  writeIORef gst gsraw {gamecard = (setPlayer pn gstate (fst marked))
                       ,cplayer = (increment pn)}
  return (gst,snd marked)