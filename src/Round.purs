-- / Game round
module App.Round where

import Data.Maybe (Maybe(..))
import App.Board as Board
import App.Turn as Turn

data State
  = Sowing Turn.State
  | Awaiting Board.Player Board.State

data Action
  = TurnAction Turn.Action
  | PlayerSelect Board.PitRef

init :: Board.Player -> Board.State -> State
init player = Awaiting player

-- XXX: Should this be made a type class?
getBoard :: State -> Board.State
getBoard (Sowing turnA) = turnA.current.board
getBoard (Awaiting _ board) = board

update :: Action -> State -> State
update (TurnAction action) (Sowing turnA) =
  -- XXX: can this pattern be abstracted out?
  -- "update inner state, but if it is Nothing do this"
  case Turn.update action turnA of
    Nothing -> -- End of turn.
      -- TODO: sould we end the round itself?
      let opponent = Board.opponentOf turnA.current.hand.player
      in Awaiting opponent turnA.current.board
    Just turnA' ->
      Sowing turnA'
update (PlayerSelect pitRef) (Awaiting player board) =
  Sowing (Turn.init player board)
update _ state =
  -- TODO: make this state transition impossible.
  state
