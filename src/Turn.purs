module App.Turn (unfoldTurns) where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Function (apply)
import App.Hand (State(..))
import App.Board as Board
import Prelude (($), (#), (+), (-), (<<<), (>>>), map)

type Turn = (State -> State)

unfoldTurns :: State -> List Turn
unfoldTurns = concat <<< unfoldr' nextTurns

nextTurns :: State -> Tuple (List Turn) (Maybe State)
nextTurns (State state@{ player, seeds, pitRef, board }) =
  Board.mapPit3 pitRef (f seeds) board
    -- TODO: fill in these functions
    where f 0 0 0 _ =
            -- No hand, next two pits empty. End turn.
            Nil # end
          f 0 0 _ 0 =
            -- Capture and end turn 
            advance : capture : Nil # end
          f 0 0 _ _ =
            -- Capture and continue
            advance : capture : advance : Nil # continue
          f 0 _ _ _ =
            -- Lift and continue digging 
            lift : advance : Nil # continue
          f _ 3 _ _ =
            -- Pasu; capture
            sow : capture : advance : Nil # continue
          f _ _ _ _ =
            -- Sow 1 seed and continue digging 
            sow : advance : Nil # continue
          continue xs =
            Tuple xs $ Just $ applyTurns xs $ State state
          end xs =
            Tuple xs Nothing

applyTurns :: List Turn -> State -> State
applyTurns turns s = foldr apply s turns

-- All turns

advance :: Turn
advance (State s) =
  State $ s { pitRef = Board.nextRef s.pitRef }

capture :: Turn
capture (State s) =
  State $ s { board = f s.board }
  where f = Board.clear s.pitRef >>> Board.store s.player seeds
        seeds = Board.lookup s.pitRef s.board

lift :: Turn 
lift (State s) =
  State $ s { seeds = seeds, board = board }
    where seeds = Board.lookup s.pitRef s.board 
          board = Board.clear s.pitRef s.board

sow :: Turn 
sow (State s) =
  State $ s { seeds = seeds, board = board }
    where seeds = s.seeds - 1
          board = Board.modify s.pitRef ((+) 1) s.board

-- Internal

-- | A version of unfoldr that allows a elements in end case.
unfoldr' :: forall a b t. Unfoldable t
         => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
