module App.Board where

import Data.Array (mapWithIndex)
import Data.Function ((#))
import Prelude (bind, const, show, ($), (<<<))
import Pux.CSS (backgroundColor, boxSizing, borderBox, display, em, inline, padding, rgb, style)
import Pux.Html (Html, div, hr, text)
import Pux.Html.Events (onClick)
import App.FixedMatrix72 as FM

data Action 
  = Reset 
  | Move Player Int

type Cell = Int

type State =
  { cells :: FM.FixedMatrix72 Cell
  , nextMove :: Player
  }

data Player = A | B

toRow :: Player -> FM.Row 
toRow A = FM.A
toRow B = FM.B

playerCells :: Player -> State -> Array Cell
playerCells player = FM.getRow (toRow player) <<< _.cells

init :: State
init =
  { cells: FM.init 6 
  , nextMove: A
  }

update :: Action -> State -> State
update Reset state = 
  init
update (Move player idx) state = 
  state

view :: State -> Html Action
view state =
  div []
  [ div [] $ viewRow A
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow B
  ]
  where 
    viewRow player = 
      mapWithIndex (viewCell <<< Move player) (playerCells player state)

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
