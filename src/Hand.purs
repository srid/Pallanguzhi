-- / What is in the hand during a round
module App.Hand where

import Data.Maybe
import App.Board as Board
import App.Board (class HasBoard)
import Data.Function (apply)
import Data.List (List(..), (:), concat, foldr)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prelude ((>>>), (<<<), ($), map)

newtype State = State
  { player :: Board.Player
  , seeds :: Board.Cell
  , pitRef :: Board.PitRef
  , board :: Board.State
  }

instance hasBoardHand :: HasBoard State where
  getBoard (State h) = h.board

init :: Board.Player -> Board.PitRef -> Board.State -> State
init player pitRef board = State
  { player: player
  , seeds: 0
  , pitRef: pitRef
  , board: board
  }

opponent :: State -> Board.Player
opponent (State { player }) = Board.opponentOf player

unfoldTurns :: State -> List (State -> State)
unfoldTurns = concat <<< unfoldr' nextTurns

nextTurns :: State -> Tuple (List (State -> State)) (Maybe State)
nextTurns (State state@{ player, seeds, pitRef, board }) =
  go seeds
     (Board.lookup pitRef board)
     (Board.lookup (Board.nextRef pitRef) board)
     (Board.lookup ((Board.nextRef <<< Board.nextRef) pitRef) board)
     state
    where continue xs = Tuple xs (Just $ applyTurns xs $ State state)
          end xs = Tuple xs Nothing
          go 0 0 0 _ state' =
            end $ Nil
          go 0 0 s _ state' =
            end $ advance : capture : Nil
          go _ _ _ _ state' =
            end $ Nil -- TODO: signal not reachable state

applyTurns :: List (State -> State) -> State -> State
applyTurns turns s = foldr apply s turns

advance :: State -> State
advance state =
  state -- TODO

capture :: State -> State
capture (State state@{ player, seeds, pitRef, board }) =
  State $ state { board = board' }
  where board' = ( Board.clear pitRef >>> Board.store player seeds' ) board
        seeds'  = Board.lookup pitRef board
-- | A version of unfoldr that allows a elements in end cause.
unfoldr' :: forall a b t. Unfoldable t
        => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
