module App.BoardView where 

import App.FixedMatrix72 as FM
import App.Board (Pit, PitRef, Player, Board)
import App.FixedMatrix72 (Row(B, A))
import Prelude (bind, const, show, ($), (<<<), (<>))
import Pux.CSS (Color, backgroundColor, boxSizing, borderBox, display, em, inline, padding, rgb, style)
import Pux.Html (Html, div, hr, text, (#), (!))
import Pux.Html.Events (onClick)

class BoardView a where 
  getBoard :: a -> Board 
  isPlaying :: a -> Player -> Boolean
  pitState :: a -> PitRef -> PitState 

data PitState = Normal | Lifted | Captured | Sowed

pitStateColor :: PitState -> Color 
pitStateColor Normal = rgb 100 200 100
pitStateColor Captured = rgb 300 0 0
pitStateColor Lifted = rgb 200 200 300
pitStateColor Sowed = rgb 100 300 300

view :: forall action state. BoardView state 
     => (PitRef -> action) -> state -> Html action 
view f state =
  div []
  [ viewStore state A
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow A
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow B
  , hr [] [] -- XXX: remove this ugly display hack
  , viewStore state B
  ]
  where
    board = getBoard state
    viewRow player = FM.mapRowWithIndex player viewCell' board.cells
      where viewCell' ref = viewCell state (f ref) ref 

viewStore :: forall a state. BoardView state 
          => state -> Row -> Html a
viewStore state player = 
  div ! css # text $ "Player " <> show player
    where css = style $ do 
                  backgroundColor color 
          color = if isPlaying state player 
                  then focusColor 
                  else rgb 198 34 112

viewCell :: forall a state. BoardView state 
          => state -> a -> PitRef -> Pit -> Html a
viewCell state action ref count =
  div ! css ! event # body
  where
    body = text $ show count
    event = onClick $ const action
    css = style $ do
      display inline
      backgroundColor $ pitColor state ref
      squarePadding (em 1.0)
      boxSizing borderBox
      where
        squarePadding sz = padding sz sz sz sz

pitColor :: forall state. BoardView state 
         => state -> PitRef -> Color 
pitColor state = pitStateColor <<< pitState state 

focusColor :: Color
focusColor = rgb 100 200 100