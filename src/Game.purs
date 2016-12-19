module App.Game where

import App.Board as Board
import App.Round as Round
import App.BoardView as BoardView
import App.BoardView (class BoardView, getBoard, getCurrentPlayer, getHand, getTurn, getPitAction)
import App.FixedMatrix72 (Row(..))
import Prelude (($), (<$>), (#))
import Data.Maybe
import Data.Either (Either(..))
import Pux.Html (Html)
import Pux.Html as H
import Pux (EffModel, mapEffects, mapState, noEffects)

data State
  = PlayingRound Round.State
  | EndRound Board.Board

data Action
  = RoundAction Round.Action
  | NextRound

instance boardViewGame :: BoardView State Action where
  getBoard (PlayingRound round) = getBoard round
  getBoard (EndRound board) = board

  getCurrentPlayer (PlayingRound round) = getCurrentPlayer round
  getCurrentPlayer _ = Nothing

  getHand (PlayingRound round) = getHand round
  getHand _ = Nothing

  getTurn (PlayingRound round) = getTurn round
  getTurn _ = Nothing

  getPitAction (PlayingRound round) ref = RoundAction <$> getPitAction round ref
  getPitAction _ _ = Nothing

init :: State
init = PlayingRound $ Round.init A Board.init

update :: forall eff. Action -> State -> EffModel State Action (eff)
update (RoundAction action) (PlayingRound round) =
  case Round.update action round of
    Right result ->
      result
      # mapEffects RoundAction
      # mapState PlayingRound
    Left board -> -- Round over
      EndRound board
      # noEffects

update NextRound (EndRound board) =
  Round.init A board -- TODO
  # PlayingRound
  # noEffects

update _ state =
  -- TODO: make this not possible
  state
  # noEffects

view :: State -> Html Action
view (PlayingRound round) =
  H.div []
    [ H.h2 [] [ H.text "Playing round #1" ]
    , RoundAction <$> Round.view round
    ]
view state =
  H.div []
    [ H.h2 [] [ H.text "Round over" ]
    , BoardView.view state
    ]
