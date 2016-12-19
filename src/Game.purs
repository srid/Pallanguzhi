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

type Config =
  { fastTurn :: Boolean
  }

data State
  = PlayingRound Config Round.State
  | EndRound Config Board.Board

data Action
  = RoundAction Round.Action
  | NextRound

instance boardViewGame :: BoardView State Action where
  getBoard (PlayingRound _ round) = getBoard round
  getBoard (EndRound _ board) = board

  getCurrentPlayer (PlayingRound _ round) = getCurrentPlayer round
  getCurrentPlayer _ = Nothing

  getHand (PlayingRound _ round) = getHand round
  getHand _ = Nothing

  getTurn (PlayingRound _ round) = getTurn round
  getTurn _ = Nothing

  getPitAction (PlayingRound _ round) ref = RoundAction <$> getPitAction round ref
  getPitAction _ _ = Nothing

init :: State
init = PlayingRound config $ Round.init A Board.init
        where config = { fastTurn: false }

update :: forall eff. Action -> State -> EffModel State Action (eff)
update (RoundAction action) (PlayingRound config round) =
  case Round.update action round of
    Right result ->
      result
      # mapEffects RoundAction
      # mapState (PlayingRound config)
    Left board -> -- Round over
      EndRound config board
      # noEffects

update NextRound (EndRound config board) =
  Round.init A board -- TODO
  # PlayingRound config
  # noEffects

update _ state =
  -- TODO: make this not possible
  state
  # noEffects

view :: State -> Html Action
view (PlayingRound _ round) =
  H.div []
    [ H.h2 [] [ H.text "Playing round #1" ]
    , RoundAction <$> Round.view round
    ]
view state =
  H.div []
    [ H.h2 [] [ H.text "Round over" ]
    , BoardView.view state
    ]
