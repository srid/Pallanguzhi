module App.Board where

import Matrix as Matrix
import CSS (inline)
import Data.Function ((#))
import Data.Maybe (fromMaybe)
import Matrix (Matrix)
import Prelude (($), show, map, bind)
import Pux.CSS (display, em, padding, rgb, backgroundColor, style)
import Pux.Html (Html, div, span, text)

data Action = Reset | Move Player

type Cell = Int

type State =
  { cells :: Matrix Cell
  , nextMove :: Player }

data Player = A | B

notPossible :: Array Cell
notPossible = [99, 99, 99, 99, 99, 99, 99]

init :: State
init =
  { cells: Matrix.repeat 7 2 0
  , nextMove: A }

update :: Action -> State -> State
update Reset state = init
update (Move player) state = state

view :: State -> Html Action
view state =
  div []
  [ div [] $ map viewCell $ row 0
  , div [] [ text "sep" ] -- XXX: remove this ugly display hack 
  , div [] $ map viewCell $ row 1
  ]
  where
    row n = fromMaybe notPossible $ Matrix.getRow n state.cells

viewCell :: Cell -> Html Action
viewCell count =
  div [s] [ text $ show count ]
  where
    s = style $ do
      display inline
      backgroundColor (rgb 200 100 0)
      squarePadding (1.0 # em)
    squarePadding sz = padding sz sz sz sz
