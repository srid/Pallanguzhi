module App.Turn (unfoldTurns) where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Function (apply)
import App.Hand (State(..))
import App.Board as Board
import Prelude (($), (#), (<<<), (>>>), map)

type Turn = (State -> State)

unfoldTurns :: State -> List Turn
unfoldTurns = concat <<< unfoldr' nextTurns

nextTurns :: State -> Tuple (List Turn) (Maybe State)
nextTurns (State state@{ player, seeds, pitRef, board }) =
  Board.mapPit3 pitRef (f state seeds) board
    -- TODO: fill in these functions
    where f state' 0 0 0 _ =
            Nil # end
          f state' 0 0 s _ =
            advance : capture : Nil # end
          f state' _ _ _ _ =
            Nil # end
          continue xs =
            Tuple xs $ Just $ applyTurns xs $ State state
          end xs =
            Tuple xs Nothing

applyTurns :: List Turn -> State -> State
applyTurns turns s = foldr apply s turns

-- All turns

advance :: Turn
advance state =
  state -- TODO

capture :: Turn
capture (State state@{ player, seeds, pitRef, board }) =
  State $ state { board = f board }
  where f = Board.clear pitRef >>> Board.store player seeds'
        seeds' = Board.lookup pitRef board

-- Internal

-- | A version of unfoldr that allows a elements in end case.
unfoldr' :: forall a b t. Unfoldable t
         => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
