{-# LANGUAGE NamedFieldPuns #-}

module Game
  ( availableMoves,
    new,
    makeMove,
    status,
    rowsAsStrings,
    Move,
    Status (..),
    Outcome (..),
    Player (..),
    Game,
  )
where

import Control.Applicative ((<|>))
import qualified Data.List as List
import Data.Maybe

data Game = Game
  -- Rows stored in reverse order (bottom to top). I think it would be better if we store columns instead of rows as there are more operations over columns than rows
  { rows :: [Row],
    nextPlayer :: Player,
    adjacentToWin :: Int
  }

data Player
  = Nought
  | Cross
  deriving (Show, Eq)

data Slot
  = Empty
  | Taken Player
  deriving (Eq)

type Line = [Slot]

type Row = Line

type Column = Line

type Diagonal = Line

instance Show Slot where
  show Empty = "."
  show (Taken Nought) = "o"
  show (Taken Cross) = "x"

type Move = Int

data Status
  = Ongoing Player
  | Finished Outcome
  deriving (Eq, Show)

data Outcome = Win Player | Draw
  deriving (Eq, Show)

new :: Int -> Int -> Int -> Game
new rowNum colNum adjacentToWin =
  Game
    { rows = buildRows rowNum colNum,
      nextPlayer = Nought,
      adjacentToWin = adjacentToWin
    }

buildRows :: Int -> Int -> [Row]
buildRows rowNum colNum = replicate rowNum row
  where
    row = replicate colNum Empty

status :: Game -> Status
status game =
  maybe notFinished (Finished . Win) (findWinner game)
  where
    notFinished = if isOngoing game then Ongoing (nextPlayer game) else Finished Draw

availableMoves :: Game -> [Move]
availableMoves Game {rows} =
  map fst
    . filter (hasBlanks . snd)
    . zip [0 ..]
    $ toColumns rows
  where
    hasBlanks = elem Empty

isOngoing :: Game -> Bool
isOngoing Game {rows} = any (elem Empty) rows

findWinner :: Game -> Maybe Player
findWinner Game {rows, adjacentToWin, nextPlayer} =
  check (toColumns rows) <|> check rows <|> check (toDiagonals rows)
  where
    check :: [Line] -> Maybe Player
    check = findWinningStreak . fmap (filterCurrentPlayer . List.group)
    filterCurrentPlayer = List.filter (all (== Taken currentPlayer))
    currentPlayer = if nextPlayer == Nought then Cross else Nought
    findWinningStreak :: [[Line]] -> Maybe Player
    findWinningStreak slots = if any ((== adjacentToWin) . length) (mconcat slots) then Just currentPlayer else Nothing

makeMove :: Move -> Game -> Maybe Game
makeMove move game@Game {nextPlayer} =
  Just
    . switchPlayer
    . updateRows (updateSlot move nextPlayer)
    $ game

updateSlot :: Move -> Player -> [Row] -> [Row]
updateSlot move player =
  fromColumns
    . replaceNth move (addPlayer player)
    . toColumns

addPlayer :: Player -> Column -> Column
addPlayer player =
  replaceFirst Empty (Taken player)

switchPlayer :: Game -> Game
switchPlayer Game {rows, nextPlayer = Nought, adjacentToWin} =
  Game
    { rows = rows,
      nextPlayer = Cross,
      adjacentToWin
    }
switchPlayer Game {rows, nextPlayer = Cross, adjacentToWin} =
  Game
    { rows = rows,
      nextPlayer = Nought,
      adjacentToWin
    }

toColumns :: [Row] -> [Column]
toColumns = List.transpose

fromColumns :: [Column] -> [Row]
fromColumns = List.transpose

updateRows :: ([Row] -> [Row]) -> Game -> Game
updateRows update Game {rows, nextPlayer, adjacentToWin} =
  Game (update rows) nextPlayer adjacentToWin

rowsAsStrings :: Game -> [String]
rowsAsStrings Game {rows} = reverse $ map toRowString rows
  where
    toRowString = mconcat . map show

-- List utils

replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst match replacement items =
  replaceNth firstFound (const replacement) items
  where
    firstFound = fromMaybe 0 $ List.elemIndex match items

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n toNewVal (x : xs)
  | n == 0 = toNewVal x : xs
  | otherwise = x : replaceNth (n -1) toNewVal xs

-- I have no idea how this works at this point - got it from https://stackoverflow.com/a/32467482
-- need to think though how it works
rotate90 :: [[a]] -> [[a]]
rotate90 = List.reverse . List.transpose

rotate180 :: [[a]] -> [[a]]
rotate180 = rotate90 . rotate90

toDiagonals :: [Row] -> [Diagonal]
toDiagonals rows =
  diagonals rows ++ diagonals (reverse rows)
  where
    diagonals r =
      (++)
        <$> List.transpose . zipWith drop [0 ..]
        <*> List.transpose
          . zipWith drop [1 ..]
          . rotate180
        $ r
