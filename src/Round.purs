-- / Game round
module App.Round where

import Data.Maybe (Maybe(..))
import App.Board as Board
import App.Turn as Turn
import Prelude ((<<<))

data State
  = Sowing Turn.State
  | Awaiting Board.Player Board.State

init :: Board.Player -> Board.State -> State
init player = Sowing <<< Turn.init player

data Action
  = TurnAction Turn.Action
  | PlayerSelect Board.PitRef 

update :: Action -> State -> State 
update (TurnAction action) (Sowing turnA) = 
  -- XXX: can this pattern be abstracted out? 
  -- "update inner state, but if it is Nothing do this"
  case Turn.update action turnA of 
    Nothing -> -- End of turn.
      -- TODO: sould we end the round itself?
      let opponent = Board.opponentOf turnA.turn.hand.player
      in Awaiting opponent turnA.turn.board
    Just turnA' ->
      Sowing turnA'
update (PlayerSelect pitRef) (Awaiting player board) =
  Sowing (Turn.init player board)
update _ state =
  -- TODO: make this state transition impossible.
  state