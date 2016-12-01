-- / Game round
module App.Round where

import Data.Maybe (Maybe(..))
import App.Board as Board
import App.Hand as Hand
import App.Animation as Animation
import Prelude (($))

data State
  = Sowing (Animation.State Hand.State)
  | Awaiting Board.Player Board.State

data Action
  = AnimateTurn
  | PlayerSelect Board.PitRef

init :: Board.Player -> Board.State -> State
init player = Awaiting player

sow :: Board.Player -> Board.PitRef -> Board.State -> State
sow player pitRef board = Sowing $ Animation.init hand rest
  where hand = Hand.init player pitRef board
        rest = Hand.unfoldTurns hand

-- XXX: Should this be made a type class?
getBoard :: State -> Board.State
getBoard (Sowing handA) = handA.current.board
getBoard (Awaiting _ board) = board

update :: Action -> State -> State
update AnimateTurn (Sowing handA) =
  -- XXX: can this pattern be abstracted out?
  -- "update inner state, but if it is Nothing do this"
  case Animation.step handA of
    Nothing -> -- End of turn.
      -- TODO: sould we end the round itself?
      let opponent = Board.opponentOf handA.current.player
      in Awaiting opponent handA.current.board
    Just handA' ->
      Sowing handA'
update (PlayerSelect pitRef) (Awaiting player board) =
  sow player pitRef board
update _ state =
  -- TODO: make this state transition impossible.
  state
