-- / Game round
module App.Round where

import Data.Maybe (Maybe(..))
import App.Board as Board
import App.Board (class HasBoard, getBoard)
import App.Hand as Hand
import App.Hand as Turn
import App.Animation as Animation
import Prelude (($), (<<<))
import Pux.Html (Html)

data State
  = Sowing (Animation.State Hand.State)
  | Awaiting Board.Player Board.State

data Action
  = AnimateTurn
  | PlayerSelect Board.PitRef

instance hasBoardRound :: HasBoard State where
  getBoard (Sowing handA) = getBoard handA.current
  getBoard (Awaiting _ board) = board

init :: Board.Player -> Board.State -> State
init player = Awaiting player

sow :: Board.Player -> Board.PitRef -> Board.State -> State
sow player pitRef board = Sowing $ Animation.init hand rest
  where hand = Hand.init player pitRef board
        rest = Turn.unfoldTurns hand

update :: Action -> State -> State
update AnimateTurn (Sowing handA) =
  -- XXX: can this pattern be abstracted out?
  -- "update inner state, but if it is Nothing do this"
  case Animation.step handA of
    Nothing -> -- End of turn.
      -- TODO: sould we end the round itself?
      let opponent = Hand.opponent handA.current
      in Awaiting opponent $ getBoard handA.current
    Just handA' ->
      Sowing handA'
update (PlayerSelect pitRef) (Awaiting player board) =
  sow player pitRef board
update _ state =
  -- TODO: make this state transition impossible.
  state

view :: State -> Html Action
view = Board.view PlayerSelect <<< getBoard
