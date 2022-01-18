module Main where

import Data.Maybe
import Game
import ComputerPlayer as Computer

main :: IO ()
main = do
  run defaultGame

defaultGame :: Game
defaultGame = Game.new 4 4 3

run :: Game -> IO ()
run game = do
  putStrLn $ "moves are: " ++ show (Game.availableMoves game)
  case Game.status game of
    Ongoing Nought -> do
      putStrLn "It's your turn o"
      printBoard game
      move <- readLn -- this is unsafe, as any non-integer input will blow up
      run $ fromMaybe defaultGame (Game.makeMove move game) -- here we could handle invalid moves
    Ongoing Cross -> do
      putStrLn "Computer's turn "
      printBoard game
      run $ fromMaybe defaultGame (Game.makeMove (Computer.bestMove game) game)
    Finished Draw -> do
      putStrLn "It was a draw!"
      printBoard game
    Finished (Win w) -> do
      putStrLn $ "We have a winner: " ++ show w
      printBoard game

printBoard :: Game -> IO ()
printBoard game =
  putStrLn (unlines $ Game.rowsAsStrings game)
