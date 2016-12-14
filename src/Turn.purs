module App.Turn (unfoldTurns, Turn(..)) where

import Data.List
import App.Board as Board
import App.TurnAnimation (class Turnable, runTurn)
import App.Hand (State(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prelude (class Show, flip, map, (#), ($), (+), (-), (<<<))

data Turn = Advance | Capture | Lift | Sow

instance showTurn :: Show Turn where 
  show Advance = "Advance"
  show Capture = "Capture"
  show Lift = "Lift"
  show Sow = "Sow"

-- XXX: keep UI config isolated
instance turnableTurn :: Turnable State Turn where 
  runTurn Advance = advance
  runTurn Capture = capture
  runTurn Lift = lift 
  runTurn Sow = sow

  turnDelay (Just Capture) _ = 500
  turnDelay _ (Just Capture) = 500
  turnDelay (Just Lift) _ = 500
  turnDelay _ (Just Lift) = 500
  turnDelay (Just Advance) _ = 100
  turnDelay (Just Sow) _ = 50
  turnDelay Nothing _ = 100

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
            Advance : Capture : Nil # end
          f 0 0 _ _ =
            -- Capture and continue
            Advance : Capture : Advance : Nil # continue
          f 0 _ _ _ =
            -- Lift and continue digging 
            Lift : Advance : Nil # continue
          f _ 3 _ _ =
            -- Pasu; capture
            Sow : Capture : Advance : Nil # continue
          f _ _ _ _ =
            -- Sow 1 seed and continue digging 
            Sow : Advance : Nil # continue
          continue xs =
            Tuple xs $ Just $ applyTurns xs $ State state
          end xs =
            Tuple xs Nothing

applyTurns :: List Turn -> State -> State
applyTurns turns s = foldl (flip runTurn) s turns

-- All turns

advance :: State -> State
advance (State s) =
  State $ s { pitRef = Board.nextRef s.pitRef }

capture :: State -> State
capture (State s) =
  State $ s { board = s.board 
                      # Board.clear s.pitRef 
                      # Board.store s.player seeds
            }
    where seeds = Board.lookup s.pitRef s.board

lift :: State -> State 
lift (State s) =
  State $ s { seeds = Board.lookup s.pitRef s.board 
            , board = Board.clear s.pitRef s.board
            }

sow :: State -> State 
sow (State s) =
  State $ s { seeds = s.seeds - 1
            , board = Board.modify s.pitRef ((+) 1) s.board
            }

-- Internal

-- | A version of unfoldr that allows a elements in end case.
unfoldr' :: forall a b t. Unfoldable t
         => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
