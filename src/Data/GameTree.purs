-- | This module defines a type class for nodes in a tree of game states
-- | (two-player zero-sum games) and provides algorithms to search the game
-- | trees for optimal strategies.
-- |
module Data.GameTree
  ( Score(..)
  , Plies
  , class Node
  , isTerminal
  , score
  , children
  , Result
  , minmax
  , bestMove
  ) where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.NonEmpty (NonEmpty(..), singleton, tail)

-- | `cons` for `NonEmpty List`s.
consNE :: ∀ a. a -> NonEmpty List a -> NonEmpty List a
consNE x (NonEmpty y ys) = NonEmpty x (y : ys)

infixr 5 consNE as :||

-- | The (heuristic) value of a node in the game tree. `Win` and `Lose` can be
-- | thought of as *+infinity* and *-infinity*.
data Score = Win | Lose | Score Number

derive instance eqScore :: Eq Score

instance ordScore :: Ord Score where
  compare Win _  = GT
  compare Lose _ = LT
  compare _ Win  = LT
  compare _ Lose = GT
  compare (Score x) (Score y) = compare x y

instance showScore :: Show Score where
  show Win       = "Win"
  show Lose      = "Lose"
  show (Score x) = "Score " <> show x

-- | Negate a `Score`.
negateScore :: Score -> Score
negateScore Win = Lose
negateScore Lose = Win
negateScore (Score x) = Score (negate x)

-- | The number of moves to look ahead.
type Plies = Int

-- | A node in a game tree, typically representing a given game state.
-- | `isTerminal` returns `true` if the node refers to a game state that is
-- | either a win, a lose, or a draw. The `score` of a node is the (heuristic)
-- | value of the given node and is always defined from the viewpoint of
-- | the 'first' player (the one who calls the search algorithm). Finally,
-- | `children` returns a list of game states that can be reached through valid
-- | moves.
-- |
-- | Law:
-- | ``` purs
-- |   isTerminal n == (null (children n))
-- | ```
class Node a where
  isTerminal :: a -> Boolean
  score :: a -> Score
  children :: a -> List a

-- | The result of a game tree search. The principal variation is a list of
-- | game states (including the root node) the would result in the given score.
type Result a = { principalVariation :: NonEmpty List a, score :: Score }

-- | An implementation of a simple MinMax algorithm (in Negamax formulation).
-- | Computes the principal variation and the best possible score for the
-- | player who is about to make a move at the given root node.
minmax :: ∀ a. Node a => Plies -> a -> Result a
minmax plies rootNode = go plies true rootNode
  where
    go :: Plies -> Boolean -> a -> Result a
    go p max node
      | p == 0 || isTerminal node =
        { principalVariation: singleton node
        , score: if max then score node else negateScore (score node) }
      | otherwise =
        { principalVariation: node :|| best.principalVariation
        , score: negateScore best.score }
      where
        best = fromJust $ minimumBy (comparing _.score)
                                    (go (p - 1) (not max) <$> children node)

-- | Returns the child node that results from the best move from a given root
-- | node in the game tree. Returns `Nothing` if the node is terminal.
-- |
-- | Example: 3-ply minmax:
-- | ``` purs
-- | bestMove rootNode (minmax 3)
-- | ```
bestMove :: ∀ a. Node a => a -> (a -> Result a) -> Maybe a
bestMove rootNode search = List.head (tail (search rootNode).principalVariation)
