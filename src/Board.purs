module App.Board where

import App.FixedMatrix72 as FM
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

data Player = A | B

newtype PitRef = PitRef { player :: Player, idx :: Int }

toRow :: Player -> FM.Row 
toRow A = FM.A
toRow B = FM.B

toRef :: PitRef -> FM.Ref 
toRef (PitRef { player, idx }) = FM.makeRef (toRow player) idx

makeRef :: Player -> Int -> PitRef 
makeRef player idx = PitRef { player, idx }

nextRef :: PitRef -> PitRef 
nextRef (PitRef r) = PitRef (f r)
  where f {player: A, idx: 6} = {player: B, idx: 6}
        f {player: B, idx: 0} = {player: A, idx: 0}
        f r' = r' {idx = r.idx + 1}

lookup :: PitRef -> State -> Cell 
lookup pitRef board =
  FM.lookup (toRef pitRef) board.cells

playerCells :: Player -> State -> Array Cell
playerCells player = FM.getRow (toRow player) <<< _.cells

init :: State
init =
  { cells: FM.init 6 
  , storeA: 0
  , storeB: 0
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
