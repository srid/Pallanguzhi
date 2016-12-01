module App.Turn where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Function (apply)
import App.Hand (State(..))
import App.Board as Board
import Prelude (($), (<<<), (>>>), map)

type Turn = (State -> State)

unfoldTurns :: State -> List Turn
unfoldTurns = concat <<< unfoldr' nextTurns

nextTurns :: State -> Tuple (List Turn) (Maybe State)
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

applyTurns :: List Turn -> State -> State
applyTurns turns s = foldr apply s turns

-- All turns

advance :: Turn
advance state =
  state -- TODO

capture :: Turn
capture (State state@{ player, seeds, pitRef, board }) =
  State $ state { board = board' }
  where board' = ( Board.clear pitRef >>> Board.store player seeds' ) board
        seeds'  = Board.lookup pitRef board

-- Internal

-- | A version of unfoldr that allows a elements in end cause.
unfoldr' :: forall a b t. Unfoldable t
        => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
