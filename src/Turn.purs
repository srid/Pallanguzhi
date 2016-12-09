module App.Turn (unfoldTurns, Turn'(..)) where

import Data.List
import App.Board as Board
import App.Animation (class Transition)
import App.Hand (State(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prelude (class Show, flip, map, (#), ($), (+), (-), (<<<))

data Turn' a
  = Advance (a -> a)
  | Capture (a -> a)
  | Lift (a -> a)
  | Sow (a -> a)

instance showTurn :: Show (Turn' a) where 
  show (Advance _) = "Advance"
  show (Capture _) = "Capture"
  show (Lift _) = "Lift"
  show (Sow _) = "Sow"

type Turn = Turn' State

instance transitionTurn :: Transition Turn' State where
  getTransitionF = turnFunction

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

turnFunction :: forall a. Turn' a -> a -> a 
turnFunction (Advance f) = f
turnFunction (Capture f) = f
turnFunction (Lift f) = f
turnFunction (Sow f) = f

applyTurns :: List Turn -> State -> State
applyTurns turns s = foldl (flip applyTurn) s turns

applyTurn :: Turn -> State -> State 
applyTurn = turnFunction 

-- All turns

advance :: Turn
advance = Advance \(State s) ->
  State $ s { pitRef = Board.nextRef s.pitRef }

capture :: Turn
capture = Capture \(State s) ->
  State $ s { board = s.board 
                      # Board.clear s.pitRef 
                      # Board.store s.player (Board.lookup s.pitRef s.board)
            }

lift :: Turn 
lift = Lift \(State s) ->
  State $ s { seeds = Board.lookup s.pitRef s.board 
            , board = Board.clear s.pitRef s.board
            }

sow :: Turn 
sow = Sow \(State s) ->
  State $ s { seeds = s.seeds - 1
            , board = Board.modify s.pitRef ((+) 1) s.board
            }

-- Internal

-- | A version of unfoldr that allows a elements in end case.
unfoldr' :: forall a b t. Unfoldable t
         => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
