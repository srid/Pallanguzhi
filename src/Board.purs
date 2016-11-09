module App.Board where

import Pux.Html (Html, div, span, text)
import Matrix as Matrix
import Matrix (Matrix)

data Action = Reset | Move Player

type State =
  { cells :: Matrix Int
  , nextMove :: Player }

data Player = A | B

init :: State
init =
  { cells: Matrix.repeat 7 2 0
  , nextMove: A }

update :: Action -> State -> State
update Reset state = init
update (Move player) state = state

view :: State -> Html Action
view state =
  div
    []
    [ span [] [ text "Hello!" ] ]
