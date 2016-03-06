module Test.Main where

import Prelude

import Data.List (List(..), (..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Ord (min)

import Data.GameTree (class Node, Score(..), minmax, bestMove)

import Test.Unit (test, runTest)
import Test.Unit.Assert (assert, equal)

-- Coins game -----------------------------------------------------------------

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

instance nodeCoinsState :: Node CoinsState where
    isTerminal (CoinsState _ n) = n == 0

    -- In this case, it is sufficient to only define the score at the terminal
    -- nodes with zero coins, because we search the tree to the lowest depth.
    score (CoinsState Alice n) | n == 0 = Win
    score (CoinsState Bob   n) | n == 0 = Lose

    children (CoinsState pl n) = do
      taken <- 1 .. min 3 n
      pure (CoinsState (next pl) (n - taken))


-- Manual test tree -----------------------------------------------------------

-- The scores in parenthesis are seen from the perspective of the player at the
-- root node.
--
--    0-ply         1-ply          2-ply         3-ply
--
--                            +---> B1 (Win)
--              +--> A1 (?) --+
--              |             +---> B2 (1)
--              |
--              |             +---> B3 (1) -+--> C1 (3)
--              |             |             |
--              |             |             +--> C2 (Lose)
--  Root (0) ---+--> A2 (?) --+
--              |             +---> B4 (4)
--              |
--              +--> A3 (2)
--
-- * On a 3-ply search (up to level C), the best possible move is A2, because
--   the expected score is 3 if the enemy plays perfectly (B3). Playing A1
--   instead would result in a score of 1, assuming a perfect response (B2).
--   Playing A3 instead immediately results in a score of 2.
--
-- * On a 2-ply search (up to level B), the seemingly best move is A3 (the A2
--   move is now valued at 1, because we do not evaluate C1).
--
-- * Scores for A1 and A2 need not be specified (they would only be evaluated
--   on a 1-ply search).

data Test = Root | A Int | B Int | C Int

derive instance eqTest :: Eq Test

instance nodeTest :: Node Test where
  isTerminal (A 3) = true
  isTerminal (B 1) = true
  isTerminal (B 2) = true
  isTerminal (B 4) = true
  isTerminal (C _) = true
  isTerminal _     = false

  score Root  = Score 0.0
  score (A 3) = Score 2.0
  score (B 1) = Win
  score (B 2) = Score 1.0
  score (B 3) = Score 1.0
  score (B 4) = Score 4.0
  score (C 1) = Score 3.0
  score (C 2) = Lose

  children Root = A 1 : A 2 : A 3 : Nil
  children (A 1) = B 1 : B 2 : Nil
  children (A 2) = B 3 : B 4 : Nil
  children (B 3) = C 1 : C 2 : Nil

main = runTest do
  let result n = (minmax 1000 (CoinsState Alice n)).score
      pv = (minmax 1000 (CoinsState Alice 4)).principalVariation

      testResult = minmax 3 Root

  test "minmax" do
    -- Search the full game tree
    equal Win  (result 0)
    equal Lose (result 1)
    equal Win  (result 2)
    equal Win  (result 3)
    equal Win  (result 4)
    equal Lose (result 5)

    assert "should find optimal strategy" $
      pv == CoinsState Alice 4 :|
            CoinsState Bob   1 :
            CoinsState Alice 0 :
            Nil

    -- Test tree
    equal (Score 3.0) testResult.score

    assert "should find optimal strategy at 3-ply" $
      (Root :| A 2 : B 3 : C 1 : Nil) == testResult.principalVariation

    assert "should find (seemingly) optimal strategy at 2-ply" $
      (Root :| A 3 : Nil) == (minmax 2 Root).principalVariation

    assert "should return the root node at 0-ply" $
      (Root :| Nil) == (minmax 0 Root).principalVariation

  test "bestMove" do
    assert "should find best move" $ Just (A 2) == bestMove (minmax 3) Root
