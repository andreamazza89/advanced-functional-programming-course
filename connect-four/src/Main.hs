module Main where

import ComputerPlayer as Computer
import Game
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  run defaultGame

defaultGame :: Game
defaultGame = Game.new 5 5 4

run :: Game -> IO ()
run game = do
  case Game.status game of
    Ongoing Nought -> do
      printBoard game
      putStrLn "It's your turn o"
      -- here we just blow up if the input is not a valid move, but could instead retry or something
      move <- fromMaybe undefined . Game.fromString game <$> getLine
      run $ Game.makeMove move game
    Ongoing Cross -> do
      printBoard game
      putStrLn "Computer's turn "
      run (Game.makeMove (Computer.bestMove game) game)
    Finished Draw -> do
      putStrLn "It was a draw!"
      printBoard game
    Finished (Win w) -> do
      putStrLn $ "We have a winner: " ++ show w
      printBoard game

printBoard :: Game -> IO ()
printBoard game =
  putStrLn (unlines $ Game.rowsAsStrings game)
