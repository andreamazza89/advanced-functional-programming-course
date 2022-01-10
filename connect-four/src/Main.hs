module Main where

import Data.Maybe
import Game

main :: IO ()
main = do
  run defaultGame

defaultGame :: Game
defaultGame = Game.new 3 3 3

run :: Game -> IO ()
run game =
  case Game.status game of
    Ongoing nextPlayer -> do
      putStrLn $ "It's your turn " ++ show nextPlayer
      printBoard game
      move <- readLn -- this is unsafe, as any non-integer input will blow up
      run $ fromMaybe defaultGame (Game.makeMove move game) -- here we could handle invalid moves
    Finished Draw -> do
      putStrLn "It was a draw!"
      printBoard game
    Finished (Win w) -> do
      putStrLn $ "We have a winner: " ++ show w
      printBoard game

printBoard :: Game -> IO ()
printBoard game = 
  putStrLn (unlines $ Game.rowsAsStrings game)