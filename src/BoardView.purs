module App.BoardView where 

import App.FixedMatrix72 as FM
import Pux.CSS as C
import App.Board (Pit, PitRef, Player, Board, getStore)
import App.Turn (Turn)
import App.Turn as Turn
import App.Hand (Hand)
import App.FixedMatrix72 (Row(B, A))
import Data.Maybe
import Prelude (bind, const, show, ($), (<>), (<), (==), pure)
import Pux.CSS (Color, backgroundColor, em, hsl, lighten, padding, px, rotateHue, style)
import Pux.Html as H
import Pux.Html (Html, div, text)
import Pux.Html.Events (onClick)

-- TODO: drop in favour of direct use of Turn
data PitState = Normal | Lifted | Captured | Sowed

class BoardView a where 
  getBoard :: a -> Board 
  getHand :: a -> Maybe Hand
  getTurn :: a -> Maybe Turn

pitState :: forall state. BoardView state => state -> PitRef -> PitState 
pitState state ref = fromMaybe Normal $ do 
  hand <- getHand state 
  turn <- getTurn state 
  if ref == hand.pitRef 
    then pure $ c turn 
    else Nothing 
      where c Turn.Capture = Captured 
            c Turn.Lift = Lifted 
            c Turn.Sow = Sowed 
            c _ = Normal

-- FIXME: not handling Awaiting state
isPlaying :: forall a. BoardView a => a -> Player -> Boolean
isPlaying a player = maybe false f (getHand a)
  where f hand = hand.player == player

pitStateColor :: PitState -> Color 
pitStateColor = go 
  where go Normal = hsl 40.0 1.0 0.3
        go Captured = rotateHue 50.0 $ go Lifted
        go Lifted = rotateHue 100.0 $ go Normal
        go Sowed = lighten 0.3 $ go Normal

view :: forall action state. BoardView state 
     => (PitRef -> action) -> state -> Html action 
view f state =
  div []
  [ viewStore state A
  , viewRow A
  , viewRow B
  , viewStore state B
  ]
  where
    board = getBoard state
    viewRow player = div [css] $ rows 
      where rows = FM.mapRowWithIndex player viewCell' board.cells
            viewCell' ref = viewCell state (f ref) ref 
            css = style $ do 
              apply4 C.padding (em 0.5)

viewStore :: forall a state. BoardView state 
          => state -> Row -> Html a
viewStore state player = div [css] [text s] 
    where s = "Player " 
                <> show player
                <> " with "
                <> show seeds
                <> " seeds."
          seeds = getStore player board
          board = getBoard state
          css = style $ do 
            backgroundColor color 
            apply4 padding (em 0.5)
          color = if isPlaying state player 
                  then pitStateColor Sowed
                  else pitStateColor Normal 

viewCell :: forall a state. BoardView state 
          => state -> a -> PitRef -> Pit -> Html a
viewCell state action ref count =
  H.pre [css, event] [body]
  where
    body = text $ numberPadded count
    event = onClick $ const action
    numberPadded num = 
      if num < 10 
        then " " <> show num 
        else show num
    css = style $ do
      C.display C.inline
      C.fontSize (em 2.0)
      C.backgroundColor color
      C.padding (em 0.0) (em padding) (em 0.0) (em padding)
      C.border C.solid (px 1.0) C.black
      where
        color = pitStateColor $ pitState state ref
        padding = 0.5

apply4 :: forall a b. (a -> a -> a -> a -> b) -> a -> b
apply4 f a = f a a a a

