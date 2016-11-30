-- / Game round
module App.Round where

import App.Board as Board
import App.Turn as Turn

type State =
  { currentTurn :: Turn.State
  }

init :: Board.Player -> Board.State -> State
init player board =
  { currentTurn: Turn.init player board }

