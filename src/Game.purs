module App.Game where 

import App.Board as Board
import App.Round as Round
import App.FixedMatrix72 (Row(..))
import Prelude (($), (<$>))
import Pux.Html (Html)

data State
  = PlayingRound Round.State

data Action
  = RoundAction Round.Action

init :: State 
init = PlayingRound $ Round.init A Board.init

update :: Action -> State -> State 
update (RoundAction action) (PlayingRound round) =
  PlayingRound $ Round.update action round

view :: State -> Html Action 
view (PlayingRound round) = RoundAction <$> boardView
    where boardView = Board.view Round.PlayerSelect board
          board = Round.getBoard round
