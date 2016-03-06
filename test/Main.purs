module Test.Main where

import Prelude

import Data.List ((..))
import Data.Ord (min)

import Data.GameTree (class Node, Score(..), minmax)

import Test.Unit (test, runTest)
import Test.Unit.Assert (equal)

-- | 'Coins' is a simple game with a stack coins. On their turn, players have
-- | to take between one and three coins from the stack. The player who takes
-- | the last coin(s) from the stack loses the game.
data Player = Alice | Bob

derive instance eqPlayer :: Eq Player

next :: Player -> Player
next Alice = Bob
next Bob = Alice

data CoinsState = CoinsState Player Int

derive instance eqCoinsState :: Eq CoinsState

instance nodeGameState :: Node CoinsState where
    isTerminal (CoinsState _ n) = n == 0

    -- In this case, it is sufficient to only define the score at the terminal
    -- nodes with zero coins, because we search the tree to the lowest depth.
    score (CoinsState Alice n) | n == 0 = Win
    score (CoinsState Bob   n) | n == 0 = Lose

    children (CoinsState pl n) = do
      taken <- 1 .. min 3 n
      pure (CoinsState (next pl) (n - taken))

main = do
  runTest $
    test "minmax" do
      -- Search the full game tree
      let result n = (minmax 1000 (CoinsState Alice n)).score

      equal Win  (result 0)
      equal Lose (result 1)
      equal Win  (result 2)
      equal Win  (result 3)
      equal Win  (result 4)
      equal Lose (result 5)
