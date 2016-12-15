module App.BoardView where 

import Data.Maybe
import App.FixedMatrix72 as FM
import App.Turn as Turn
import Data.Array as Array
import Pux.CSS as C
import Pux.Html as H
import App.Board (Pit, PitRef, Player, Board, getStore)
import App.FixedMatrix72 (Row(B, A))
import App.Hand (Hand)
import App.Turn (Turn)
import Data.Function (apply)
import Data.Traversable (sequence)
import Prelude (bind, const, show, ($), (<>), (<), (==), pure, (<$>), flip)
import Pux.CSS (Color, backgroundColor, em, hsl, lighten, padding, px, rotateHue, style)
import Pux.Html (Html, div, text)
import Pux.Html.Events (onClick)

-- TODO: drop in favour of direct use of Turn
data PitState = Normal | Lifted | Captured | Sowed

class BoardView state action | state -> action where 
  getBoard :: state -> Board 
  getHand :: state -> Maybe Hand
  getTurn :: state -> Maybe Turn
  getCurrentPlayer :: state -> Maybe Player
  getPitAction :: state -> Maybe (PitRef -> action)

pitState :: forall state action. BoardView state action => state -> PitRef -> PitState 
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

isPlaying :: forall state action. BoardView state action => state -> Player -> Boolean
isPlaying state player = fromMaybe false $ do 
  currentPlayer <- getCurrentPlayer state
  pure $ player == currentPlayer

pitStateColor :: PitState -> Color 
pitStateColor = go 
  where go Normal = hsl 40.0 1.0 0.3
        go Captured = rotateHue 50.0 $ go Lifted
        go Lifted = rotateHue 100.0 $ go Normal
        go Sowed = lighten 0.3 $ go Normal

view :: forall action state. BoardView state action
     => state -> Html action 
view state =
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
            viewCell' ref = viewCell state ref 
            css = style $ do 
              apply4 C.padding (em 0.5)

viewStore :: forall action state. BoardView state action
          => state -> Row -> Html action
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

viewCell :: forall action state. BoardView state action
          => state -> PitRef -> Pit -> Html action
viewCell state ref count =
  H.pre attrs [body]
  where
    body = text $ numberPadded count
    -- XXX: is this too clever?
    attrs = fromMaybe [] $ sequence $ Array.filter isJust [css, event]
    event = onClick <$> const <$> (flip apply) ref <$> getPitAction state
    numberPadded num = 
      if num < 10 
        then " " <> show num 
        else show num
    css = Just $ style $ do
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

