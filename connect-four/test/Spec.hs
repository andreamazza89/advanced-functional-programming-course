module Main where

import ComputerPlayer
import Game
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "the first player can make a move" $
      Game.rowsAsStrings
        <$> naughtMove 0 smallGame
        `shouldBe` Just
          [ "...",
            "o.."
          ]

    it "the second player adds to the next column" $
      Game.rowsAsStrings
        <$> (naughtMove 0 smallGame >>= crossMove 1)
        `shouldBe` Just
          [ "...",
            "ox."
          ]

    it "the second player adds to the same column" $
      Game.rowsAsStrings
        <$> (naughtMove 0 smallGame >>= crossMove 0)
        `shouldBe` Just
          [ "x..",
            "o.."
          ]

    it "the first player wins with a column" $
      Game.status
        <$> (naughtMove 0 smallGame >>= crossMove 1 >>= naughtMove 0)
        `shouldBe` Just (Finished (Win Nought))

    it "the first player wins with a row" $
      Game.status
        <$> ( pure smallGame
                >>= naughtMove 0
                >>= crossMove 0
                >>= naughtMove 1
            )
        `shouldBe` Just (Finished (Win Nought))

    it "the first player wins with a diagonal" $
      Game.status
        <$> ( pure smallGame
                >>= naughtMove 0
                >>= crossMove 1
                >>= naughtMove 1
            )
        `shouldBe` Just (Finished (Win Nought))

    it "draws" $
      Game.status
        <$> ( pure smallGame
                >>= naughtMove 0
                >>= crossMove 0
                >>= naughtMove 2
                >>= crossMove 2
                >>= naughtMove 1
                >>= crossMove 1
            )
        `shouldBe` Just (Finished Draw)

smallGame = Game.new 2 3 2

naughtMove = Game.makeMove

crossMove = Game.makeMove
