module App.BoardView where 

import App.FixedMatrix72 as FM
import App.Board (Pit, PitRef, Player, Board)
import App.FixedMatrix72 (Row(B, A))
import Prelude (bind, const, show, ($), (<>))
import Pux.CSS (Color, backgroundColor, em, hsl, lighten, padding, rotateHue, style)
import Pux.Html (Html, div, hr, span, text)
import Pux.Html.Events (onClick)

class BoardView a where 
  getBoard :: a -> Board 
  isPlaying :: a -> Player -> Boolean
  pitState :: a -> PitRef -> PitState 

data PitState = Normal | Lifted | Captured | Sowed

pitStateColor :: PitState -> Color 
pitStateColor = go 
  where go Normal = hsl 200.0 1.0 0.3
        go Captured = rotateHue 300.0 $ go Lifted
        go Lifted = rotateHue 200.0 $ go Normal
        go Sowed = lighten 0.3 $ go Normal

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
  div [css] [text $ "Player " <> show player]
    where css = style $ do 
            backgroundColor color 
            apply4 padding (em 0.5)
          color = if isPlaying state player 
                  then pitStateColor Sowed
                  else pitStateColor Normal 

viewCell :: forall a state. BoardView state 
          => state -> a -> PitRef -> Pit -> Html a
viewCell state action ref count =
  span [css, event] [body]
  where
    body = text $ show count
    event = onClick $ const action
    css = style $ do
      backgroundColor color
      apply4 padding (em 0.9)
      where
        color = pitStateColor $ pitState state ref

apply4 :: forall a b. (a -> a -> a -> a -> b) -> a -> b
apply4 f a = f a a a a