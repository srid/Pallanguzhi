module App.Game where

import Data.Maybe
import App.Board as Board
import App.BoardView as BoardView
import App.Config as Config
import App.Round as Round
import Pux.Html as H
import App.Board (Board, Player)
import App.BoardView (class BoardView, getBoard, getCurrentPlayer, getHand, getPitAction, getTurn)
import App.Config (Config)
import App.FixedMatrix72 (Row(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Prelude (const, show, (#), ($), (+), (<$>), (<>))
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.Html (Html)
import Pux.CSS as C
import Pux.Html.Events (onClick)

data State
  = PlayingRound Config Int Round.State
  | RoundOver Config Int Player Board
  | GameOver Config Int Player Board

data Action
  = RoundAction Round.Action
  | NextRound
  -- | NewGame

instance boardViewGame :: BoardView State Action where
  getBoard (PlayingRound _ _ round) = getBoard round
  getBoard (RoundOver _ _ _ board) = board
  getBoard (GameOver _ _ _ board) = board

  getCurrentPlayer (PlayingRound _ _ round) = getCurrentPlayer round
  getCurrentPlayer _ = Nothing

  getHand (PlayingRound _ _ round) = getHand round
  getHand _ = Nothing

  getTurn (PlayingRound _ _ round) = getTurn round
  getTurn _ = Nothing

  getPitAction (PlayingRound _ _ round) ref = RoundAction <$> getPitAction round ref
  getPitAction _ _ = Nothing

init :: State
init = PlayingRound config 1 round
  where config = Config.init
        round = Round.init A board
        board = if config.demo
                  then Board.initWith 21 49
                  else Board.init

update :: forall eff. Action -> State -> EffModel State Action (eff)
update (RoundAction action) (PlayingRound config nth round) =
  case Round.update config action round of
    Right result ->
      result
      # mapEffects RoundAction
      # mapState (PlayingRound config nth)
    Left (Tuple player board) -> -- Round over
      RoundOver config nth player board
      # noEffects

update NextRound (RoundOver config nth player board) =
  PlayingRound config (nth + 1) (Round.init player newBoard)
  # noEffects
    where newBoard = board
                     # captureAll A
                     # captureAll B
                     # (\b -> Board.initWith b.storeA b.storeB)
          captureAll player' board' =
            foldl (Board.storeFromPit player') board' (Board.refs player')

update _ state =
  -- TODO: make this not possible
  state
  # noEffects

view :: State -> Html Action
view (PlayingRound config nth round) =
  H.div []
    [ H.h2 [] [ H.text $ "Pallanguzhi round #" <> show nth ]
    , RoundAction <$> Round.view round
    , Config.view config
    ]

view state@(RoundOver config nth player board) =
  H.div []
    [ H.h2 [] [ H.text $ "Pallanguzhi round #" <> show nth <> " is over!" ]
    , viewNextLink
    , BoardView.view state
    , Config.view config
    ]
      where viewNextLink =
              H.button [onClick $ const NextRound, css]
                [H.text "Play next round"]
            css = C.style do
                    C.fontSize (C.em 3.0)

view state@(GameOver config nth player board) =
  H.div []
    [ H.h2 [] [ H.text $ "Pallanguzhi: "
                      <> show player
                      <> "won the game after "
                      <> show nth <> "rounds" ]
    , BoardView.view state
    , Config.view config
    ]
