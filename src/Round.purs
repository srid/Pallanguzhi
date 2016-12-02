-- / Game round
module App.Round where

import App.Animation as Animation
import App.Board as Board
import App.View as View
import App.Hand as Hand
import App.Turn as Turn
import App.View (class HasBoard, ViewConfig(..), getBoard, getBoardViewConfig)
import Data.Maybe (Maybe(..))
import Prelude (($))
import Pux.Html (Html)

data State
  = Sowing (Animation.State Hand.State)
  | Awaiting Board.Player Board.State

data Action
  = AnimateTurn
  | PlayerSelect Board.PitRef

instance hasBoardRound :: HasBoard State where
  getBoard (Sowing handA) = 
    getBoard handA.current
  getBoard (Awaiting _ board) = 
    board

  getBoardViewConfig (Sowing handA) =
    getBoardViewConfig handA.current
  getBoardViewConfig (Awaiting player board) =
    ViewConfig
      { focusPit: Nothing
      , focusPlayer: Just player
      }

init :: Board.Player -> Board.State -> State
init player = Awaiting player

sow :: Board.Player -> Board.PitRef -> Board.State -> State
sow player pitRef board = Sowing $ Animation.init hand rest
  where hand = Hand.init player pitRef board
        rest = Turn.unfoldTurns hand

update :: Action -> State -> State
update AnimateTurn (Sowing handA) =
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
view state = View.viewBoard PlayerSelect state