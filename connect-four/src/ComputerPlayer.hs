module ComputerPlayer where

import Control.Applicative ((<|>))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Debug.Trace
import qualified Game

data Score = Good | Ok | Bad
  deriving (Eq, Show)

type Depth = Int

-- given all the available moves, for each one
-- keep going down the game tree, until you hit the end and use that to rate the 'path'

bestMove :: Game.Game -> Game.Move
bestMove game =
  case Game.status game of
    -- could make this better by returning a Maybe Move, but we kinda 'guarantee' it won't happen with the game loop
    -- I wonder what it would be like to use phantom types to restrict this function's input to Ongoing games only?
    --   My gut says no but could be fun to try
    Game.Finished _ -> undefined
    Game.Ongoing computerPlayer ->
      (\(f, _, _) -> f) . bestOf $ rateMoves game computerPlayer 0 6

rateMoves :: Game.Game -> Game.Player -> Int -> Int -> [(Game.Move, Score, Depth)]
rateMoves game computerPlayer currentDepth maxDepth =
  rateMove game computerPlayer currentDepth maxDepth <$> Game.availableMoves game

rateMove :: Game.Game -> Game.Player -> Int -> Int -> Game.Move -> (Game.Move, Score, Depth)
rateMove game computerPlayer currentDepth maxDepth move =
  case Game.status afterMove of
    Game.Finished Game.Draw -> (move, Ok, currentDepth)
    Game.Finished (Game.Win winner) ->
      if winner == computerPlayer
        then (move, Good, currentDepth)
        else (move, Bad, currentDepth)
    Game.Ongoing nextPlayer ->
      if currentDepth == maxDepth
        then (move, Ok, maxDepth)
        else
          let nextRatedMoves = rateMoves afterMove computerPlayer (currentDepth + 1) maxDepth
           in if nextPlayer == computerPlayer
                then
                  let (_, score, d) = bestOf nextRatedMoves
                   in (move, score, d)
                else
                  let (_, score, d) = worstOf nextRatedMoves
                   in (move, score, d)
  where
    -- this is another junky point: I 'know' that we should never fail here, but it would be better if it was made
    -- impossible at the type level. Perhaps a move could be an opaque type and then we can remove the 'Maybe' from
    -- the makeMove if only the moves that are generated by the game can be used to make a move.
    afterMove = fromMaybe undefined $ Game.makeMove move game

bestOf :: [(Game.Move, Score, Depth)] -> (Game.Move, Score, Depth)
bestOf ratedMoves =
  fromMaybe (fromMaybe (head ratedMoves) (Just (List.minimumBy foo ratedMoves))) $
    firstGoodMove <|> firstOkMove
  where
    firstGoodMove = List.find ((== Good) . \(_, s, _) -> s) ratedMoves
    firstOkMove = List.find ((== Ok) . \(_, s, _) -> s) ratedMoves

worstOf :: [(Game.Move, Score, Depth)] -> (Game.Move, Score, Depth)
worstOf ratedMoves =
  fromMaybe (fromMaybe (head ratedMoves) (Just (List.minimumBy foo ratedMoves))) $
    firstBadMove <|> firstOkMove
  where
    firstBadMove = List.find ((== Bad) . \(_, s, _) -> s) ratedMoves
    firstOkMove = List.find ((== Ok) . \(_, s, _) -> s) ratedMoves

foo :: (Game.Move, Score, Depth) -> (Game.Move, Score, Depth) -> Ordering
foo (_, _, d) (_, _, d') = compare d d'