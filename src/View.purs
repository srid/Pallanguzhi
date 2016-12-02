module App.View where 

import Data.Maybe (Maybe)
import Prelude (bind, const, show, ($))
import Pux.CSS (backgroundColor, boxSizing, borderBox, display, em, inline, padding, rgb, style)
import Pux.Html (Html, div, hr, text, (#))
import Pux.Html.Events (onClick)
import App.Board (Cell, PitRef, Player, State)
import App.FixedMatrix72 as FM
import App.FixedMatrix72 (Row(B, A))

class HasBoard a where
  getBoard :: a -> State
  getBoardViewConfig :: a -> ViewConfig

newtype ViewConfig = ViewConfig
  { focusPit :: Maybe PitRef
  , focusPlayer :: Maybe Player
  }

viewBoard :: forall action state. HasBoard state 
          => (PitRef -> action) -> state -> Html action 
viewBoard f state =
  div []
  [ div # text "Player A"
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow A
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow B
  , hr [] [] -- XXX: remove this ugly display hack
  , div # text "Player B"
  ]
  where
    viewRow player = FM.mapRowWithIndex player viewCell' board.cells
      where viewCell' ref = viewCell config (f ref) ref 
            board = getBoard state
            config = getBoardViewConfig state

viewCell :: forall a. ViewConfig -> a -> PitRef -> Cell -> Html a
viewCell config action ref count =
  div [design, onClick (const action)] [ text content ]
  where
    content = show count
    design = style $ do
      display inline
      backgroundColor (rgb 200 100 0)
      squarePadding (em 1.0)
      boxSizing borderBox
      where
        squarePadding sz = padding sz sz sz sz
