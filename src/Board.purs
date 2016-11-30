module App.Board where

import App.FixedMatrix72 as FM
import App.FixedMatrix72 (Ref(Ref), Row(..))
import Data.Array (mapWithIndex)
import Data.Function ((#))
import Prelude ((+), bind, const, show, ($), (<<<))
import Pux.CSS (backgroundColor, boxSizing, borderBox, display, em, inline, padding, rgb, style)
import Pux.Html (Html, div, hr, text)
import Pux.Html.Events (onClick)

data Action 
  = Reset 
  | Move Player Int

-- TODO: rename to Pit
type Cell = Int

type State =
  { cells :: FM.FixedMatrix72 Cell
  , storeA :: Int
  , storeB :: Int
  }

type Player = FM.Row

type PitRef = Ref

makeRef :: Player -> Int -> PitRef 
makeRef = FM.makeRef 

nextRef :: PitRef -> PitRef 
nextRef (Ref { row: A, idx: 6 }) = Ref { row: B, idx: 6 }
nextRef (Ref { row: B, idx: 0 }) = Ref { row: A, idx: 0 }
nextRef (Ref r) = Ref $ r { idx = r.idx + 1 }

lookup :: PitRef -> State -> Cell 
lookup ref board = FM.lookup ref board.cells

playerCells :: Player -> State -> Array Cell
playerCells player = FM.getRow player <<< _.cells

opponentOf :: Player -> Player
opponentOf A = B
opponentOf B = A

init :: State
init =
  { cells: FM.init 6 
  , storeA: 0
  , storeB: 0
  }

modify :: PitRef -> (Cell -> Cell) -> State -> State 
modify ref f board = board { cells = cells' }
   where cells' = FM.modify ref f board.cells

clear :: PitRef -> State -> State 
clear pitRef = modify pitRef (const 0) 

store :: Player -> Cell -> State -> State 
store A seeds state = state { storeA = state.storeA + seeds }
store B seeds state = state { storeA = state.storeB + seeds }

-- Update

update :: Action -> State -> State
update Reset state = 
  init
update (Move player idx) state = 
  state

-- View 

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
