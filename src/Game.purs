module App.Game where

import App.Board as Board
import App.Round as Round
import App.FixedMatrix72 (Row(..))
import Prelude (($), (<$>), (#))
import Pux.Html (Html)
import Pux (EffModel, mapEffects, mapState)

data State
  = PlayingRound Round.State

data Action
  = RoundAction Round.Action

init :: State
init = PlayingRound $ Round.init A Board.init

update :: forall eff. Action -> State -> EffModel State Action (eff)
update (RoundAction action) (PlayingRound round) =
  Round.update action round
  # mapEffects RoundAction
  # mapState PlayingRound

view :: State -> Html Action
view (PlayingRound round) = RoundAction <$> Round.view round
