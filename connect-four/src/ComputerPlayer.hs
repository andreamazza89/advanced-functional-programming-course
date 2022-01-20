module ComputerPlayer where

import qualified Data.List as List
import qualified Game

-- Depth

data Depth = Depth
  { currentDepth :: Int,
    maximumDepth :: Int
  }

increaseDepth :: Depth -> Depth
increaseDepth depth =
  Depth
    { currentDepth = currentDepth depth + 1,
      maximumDepth = maximumDepth depth
    }

tooDeep :: Depth -> Bool
tooDeep depth =
  currentDepth depth >= maximumDepth depth

maxDepth :: Int -> Depth
maxDepth = Depth 0

-- Scoring a move

data Score = Bad | Ok | Good
  deriving (Eq, Show, Ord)

type ScoredMove = (Game.Move, Score)

getMove :: ScoredMove -> Game.Move
getMove = fst

getScore :: ScoredMove -> Score
getScore = snd

bestMove :: Game.Game -> Game.Move
bestMove game =
  case Game.status game of
    -- could make this better by returning a Maybe Move instead of exploding, but we kinda 'guarantee' it won't happen with the game loop
    -- I wonder what it would be like to use phantom types to restrict this function's input to Ongoing games only?
    --   My gut says no but could be fun to try
    Game.Finished _ -> undefined
    Game.Ongoing computerPlayer ->
      getMove . bestOf $ rateMoves game computerPlayer (maxDepth 6)

rateMoves :: Game.Game -> Game.Player -> Depth -> [ScoredMove]
rateMoves game computerPlayer depth =
  rateMove game computerPlayer depth <$> Game.availableMoves game

rateMove :: Game.Game -> Game.Player -> Depth -> Game.Move -> ScoredMove
rateMove game computerPlayer depth move =
  case Game.status afterMove of
    Game.Finished Game.Draw -> (move, Ok)
    Game.Finished (Game.Win winner) ->
      if winner == computerPlayer
        then (move, Good)
        else (move, Bad)
    Game.Ongoing nextPlayer ->
      if tooDeep depth
        then (move, Ok)
        else
          let nextRatedMoves = rateMoves afterMove computerPlayer (increaseDepth depth)
           in if nextPlayer == computerPlayer
                then (,) move . getScore . bestOf $ nextRatedMoves
                else (,) move . getScore . worstOf $ nextRatedMoves
  where
    afterMove = Game.makeMove move game

bestOf :: [ScoredMove] -> ScoredMove
bestOf =
  List.maximumBy compareScores

worstOf :: [ScoredMove] -> ScoredMove
worstOf =
  List.minimumBy compareScores

compareScores :: ScoredMove -> ScoredMove -> Ordering
compareScores m m' =
  compare (getScore m) (getScore m')
