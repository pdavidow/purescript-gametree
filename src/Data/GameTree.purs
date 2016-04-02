-- | This module defines a type class for nodes in a tree of game states
-- | (two-player zero-sum games) and provides algorithms to search the game
-- | trees for optimal strategies.

module Data.GameTree
  ( Score(..)
  , Plies
  , class Node
  , isTerminal
  , score
  , children
  , isTerminalDefault
  , Result
  , minmax
  , bestMove
  ) where

import Prelude

import Data.List (List, (:), null)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.NonEmpty (NonEmpty(..), singleton, tail)

-- | `cons` for `NonEmpty List`s.
consNonEmpty ∷ ∀ a. a → NonEmpty List a → NonEmpty List a
consNonEmpty x (NonEmpty y ys) = NonEmpty x (y : ys)

infixr 5 consNonEmpty as <:>

-- | The (heuristic) value of a node in the game tree. `Lose` and `Win` can
-- | be thought of as *-infinity* and *+infinity*.
-- |
-- | Mathematically, `Score` is analogous to the extended real number line,
-- | which is a totally ordered set (see `Ord` instance). `Score` is not a
-- | `Semiring`, but supports negation (`negateScore`).
-- |
-- | See: https://en.wikipedia.org/wiki/Extended_real_number_line
data Score = Lose | Score Number | Win

derive instance eqScore ∷ Eq Score
derive instance ordScore ∷ Ord Score

instance showScore ∷ Show Score where
  show Win       = "Win"
  show Lose      = "Lose"
  show (Score x) = "Score " <> show x

-- | Negate a `Score`.
negateScore ∷ Score → Score
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
  isTerminal ∷ a → Boolean
  score      ∷ a → Score
  children   ∷ a → List a

-- | A default implementation for `isTerminal`.
isTerminalDefault ∷ ∀ a. Node a ⇒ a → Boolean
isTerminalDefault = null <<< children

-- | The result of a game tree search. The principal variation is a list of
-- | game states (including the root node) the would result in the given score.
type Result a =
  { principalVariation ∷ NonEmpty List a
  , score ∷ Score }

-- | Negates the score, if the first argument is `false`.
signed ∷ Boolean → Score → Score
signed true  = id
signed false = negateScore

-- | An implementation of a simple MinMax algorithm (in Negamax formulation).
-- | Computes the principal variation and the best possible score for the
-- | player who is about to make a move at the given root node.
minmax ∷ ∀ a. Node a ⇒ Plies → a → Result a
minmax plies rootNode = go plies true rootNode
  where
    go ∷ Plies → Boolean → a → Result a
    go p maximizing node
      | p == 0 || isTerminal node =
        { principalVariation: singleton node
        , score:              signed maximizing (score node)
        }
      | otherwise =
        { principalVariation: node <:> best.principalVariation
        , score:              negateScore best.score
        }
      where
        best = fromJust $ minimumBy (comparing _.score)
                                    (go (p - 1) (not maximizing) <$> children node)

alphaBeta ∷ ∀ a. Node a ⇒ Plies → a → Result a
alphaBeta plies rootNode = go plies Lose Win true rootNode
  where
    go ∷ Plies → Score → Score → Boolean → a → Result a
    go p alpha beta maximizing node
      | p == 0 || isTerminal node =
        { principalVariation: singleton node
        , score:              signed maximizing (score node)
        }
      | otherwise =
        { principalVariation: node <:> best.principalVariation
        , score:              negateScore best.score
        }
      where
        best = fromJust $ minimumBy (comparing _.score)
                                    (go (p - 1) alpha beta (not maximizing) <$> children node)

-- | Returns the child node that results from the best move from a given root
-- | node in the game tree. Returns `Nothing` if the node is terminal.
-- |
-- | Example: 3-ply minmax:
-- | ``` purs
-- | bestMove (minmax 3) rootNode
-- | ```
bestMove ∷ ∀ a. Node a ⇒ (a → Result a) → a → Maybe a
bestMove search rootNode = List.head (tail (search rootNode).principalVariation)
