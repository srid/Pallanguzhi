module App.Board where

import Matrix as Matrix
import Data.Array (mapWithIndex)
import Data.Function ((#))
import Data.Maybe (fromJust)
import Matrix (Matrix)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, map, show, const, ($), (<<<))
import Pux.CSS (backgroundColor, boxSizing, borderBox, display, em, inline, padding, rgb, style)
import Pux.Html (Html, div, hr, text)
import Pux.Html.Events (onClick)

data Action = Reset | Move Player Int

type Cell = Int

type State =
  { cells :: Matrix Cell
  , nextMove :: Player
  }

data Player = A | B

playerACells :: State -> Array Cell
playerACells = unsafePartial fromJust <<< Matrix.getRow 0 <<< (\r -> r.cells)

playerBCells :: State -> Array Cell
playerBCells = unsafePartial fromJust <<< Matrix.getRow 1 <<< (\r -> r.cells)

init :: State
init =
  { cells: Matrix.repeat 7 2 12
  , nextMove: A
  }

update :: Action -> State -> State
update Reset state = init
update (Move player idx) state = state

view :: State -> Html Action
view state =
  div []
  [ div [] $ mapWithIndex (viewCell <<< Move A) (playerACells state)
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ mapWithIndex (viewCell <<< Move B) (playerBCells state)
  ]

viewCell :: Action -> Cell -> Html Action
viewCell action count =
  div [design, onClick (const action)] [ text content ]
  where
    content = show count
    design = style $ do
      display inline
      backgroundColor (rgb 200 100 0)
      squarePadding (1.0 # em)
      boxSizing borderBox
      where 
        squarePadding sz = padding sz sz sz sz
