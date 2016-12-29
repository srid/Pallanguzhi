module App.Turn where

import Data.List
import App.Board as Board
import App.Board (Board, Player)
import App.FixedMatrix72 (Ref(..), getRow)
import App.Hand (Hand)
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prelude (class Eq, class Show, eq, flip, map, show, (#), ($), (+), (-), (<<<), (<>), (==))

data Turn = Advance | Capture Player | Lift | Sow

type State = Tuple Hand Board

instance showTurn :: Show Turn where
  show Advance = "Advance"
  show (Capture player) = "Capture:" <> show player
  show Lift = "Lift"
  show Sow = "Sow"

instance eqTurn :: Eq Turn where
  eq Advance Advance = true
  eq (Capture p1) (Capture p2) = p1 == p2
  eq Lift Lift = true
  eq Sow Sow = true
  eq _ _ = false

runTurn :: Turn -> State -> State
runTurn Advance = advance
runTurn (Capture player) = capture player
runTurn Lift = lift
runTurn Sow = sow

unfoldTurns :: State -> List Turn
unfoldTurns = concat <<< unfoldr' nextTurns

nextTurns :: State -> Tuple (List Turn) (Maybe State)
nextTurns state@(Tuple hand@{player, seeds, pitRef} board) =
  Board.mapPit3 pitRef (f seeds) board
    -- TODO: fill in these functions
    where f 0 0 0 _ =
            -- No hand, next two pits empty. End turn.
            Nil # end
          f 0 0 _ _ =
            -- Capture and end turn
            Advance : Capture hand.player : Nil # end
          f 0 _ _ _ =
            -- Lift and continue digging
            Lift : Advance : Nil # continue
          f _ 3 _ _ =
            -- Pasu; capture
            Sow : Capture (getPlayer hand.pitRef) : Advance : Nil # continue
              where getPlayer (Ref { row, idx}) = row
          f _ _ _ _ =
            -- Sow 1 seed and continue digging
            Sow : Advance : Nil # continue
          continue xs =
            Tuple xs $ Just $ applyTurns xs state
          end xs =
            Tuple xs Nothing

applyTurns :: List Turn -> State -> State
applyTurns turns s = foldl (flip runTurn) s turns

run :: State -> State
run state = (applyTurns <<< unfoldTurns) state state

-- All turns

advance :: State -> State
advance (Tuple hand board) = Tuple hand' board
  where hand' = hand { pitRef = Board.nextRef hand.pitRef board }

capture :: Player -> State -> State
capture player (Tuple hand board) = Tuple hand board'
  where board' = Board.storeFromPit player board hand.pitRef

lift :: State -> State
lift (Tuple hand board) = Tuple hand' board'
  where board' = board # Board.clearPit hand.pitRef
        hand' = hand { seeds = Board.lookup hand.pitRef board }

sow :: State -> State
sow (Tuple hand board) = Tuple hand' board'
  where board' = board # Board.modifyPit hand.pitRef ((+) 1)
        hand' = hand { seeds = hand.seeds - 1 }

-- Internal

-- | A version of unfoldr that allows a elements in end case.
unfoldr' :: forall a b t. Unfoldable t
         => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just
