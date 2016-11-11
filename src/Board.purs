module App.Board where

import Matrix as Matrix
import CSS (inline)
import Data.Function ((#))
import Data.Maybe (fromJust)
import Matrix (Matrix)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, map, show, ($))
import Pux.CSS (display, em, padding, rgb, backgroundColor, style)
import Pux.Html (Html, div, text)
import Pux.Html.Attributes (value)
import Pux.Html.Events (MouseEvent, onClick)

data Action = Reset | Move MouseEvent

type Cell = Int

type State =
  { cells :: Matrix Cell
  , nextMove :: Player
  }

data Player = A | B

playerACells :: State -> Array Cell
playerACells state = unsafePartial fromJust $ Matrix.getRow 0 state.cells

playerBCells :: State -> Array Cell
playerBCells state = unsafePartial fromJust $ Matrix.getRow 1 state.cells

init :: State
init =
  { cells: Matrix.repeat 7 2 12
  , nextMove: A
  }

update :: Action -> State -> State
update Reset state = init
update (Move event) state = state

view :: State -> Html Action
view state =
  div []
  [ div [] $ map viewCell $ playerACells state
  , div [] [ text "." ] -- XXX: remove this ugly display hack
  , div [] $ map viewCell $ playerBCells state
  ]

viewCell :: Cell -> Html Action
viewCell count =
  div [st, value "Sample", onClick Move] [ text $ show count ]
  where
    st = style $ do
      display inline
      backgroundColor (rgb 200 100 0)
      squarePadding (1.0 # em)
    squarePadding sz = padding sz sz sz sz
