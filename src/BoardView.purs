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
import Data.Traversable (sequence)
import Prelude (bind, const, pure, show, ($), (<), (<$>), (<<<), (<>), (==), (#))
import Pux.CSS (Color, Display, em, hsl, px, style)
import Pux.Html (Attribute, Html, div, text)
import Pux.Html.Events (onClick)

class BoardView state action | state -> action where
  getBoard :: state -> Board
  getHand :: state -> Maybe Hand
  getTurn :: state -> Maybe Turn
  getCurrentPlayer :: state -> Maybe Player
  getPitAction :: state -> PitRef -> Maybe action

isPlaying :: forall state action. BoardView state action => state -> Player -> Boolean
isPlaying state player = fromMaybe false $ do
  currentPlayer <- getCurrentPlayer state
  pure $ player == currentPlayer

type PitState = Maybe Turn

pitState :: forall state action. BoardView state action
         => state -> PitRef -> PitState
pitState state ref = do
  hand <- getHand state
  if ref == hand.pitRef
    then getTurn state
    else Nothing

pitColor :: PitState -> Color
pitColor (Just Turn.Capture) = hsl 300.0 1.0 0.3
pitColor (Just Turn.Lift) = hsl 150.0 1.0 0.3
pitColor (Just Turn.Sow) = C.lighten 0.4 $ pitColor Nothing
pitColor _ = hsl 70.0 1.0 0.3

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
    board =
      getBoard state
    viewRow player =
      div [] $ FM.mapRowWithIndex player (viewPit state) board.cells

viewStore :: forall action state. BoardView state action
          => state -> Row -> Html action
viewStore state player =
  H.pre [css] [ text s ]
    where s = viewPlayer player <> showPadded seeds
          seeds = getStore player board
          board = getBoard state
          color = if isPlaying state player
                    then pitColor (Just Turn.Sow)
                    else pitColor Nothing # C.darken 0.1
          css = style do
            C.display C.block
            C.textAlign C.center
            C.fontSize (em 2.0)
            C.backgroundColor color
            C.width (em 5.0)
            C.margin (em 1.0) (em 0.0) (em 1.0) (em 8.5)
            C.border C.solid (px 1.0) C.black


viewPit :: forall action state. BoardView state action
        => state -> PitRef -> Pit -> Html action
viewPit state ref count =
  H.pre (getJusts [css, event]) [body]
  where
    body = text $ showPadded count
    event = onClick <$> const <$> getPitAction state ref
    color = pitColor $ pitState state ref
    css = Just $ style do
      C.display C.inlineFlex
      C.position C.relative
      C.width boxSize
      C.height boxSize
      C.textAlign C.center
      C.fontSize (em 2.5)
      C.backgroundColor color
      C.padding (em 0.0) (em padding) (em 0.0) (em padding)
      apply4 C.margin (em 0.0)
      C.border C.solid (px 1.0) C.black
      where
        padding = 0.5
        boxSize = (em 1.5)

showPadded :: Int -> String
showPadded n =
  if n < 10
    then " " <> show n <> extra
    else show n <> extra
      where extra = "  "

viewPlayer :: Player -> String
viewPlayer player = viewPlayerEmoji player

viewPlayerEmoji :: Player -> String
viewPlayerEmoji A = "🐄"
viewPlayerEmoji B = "🐓"

cellStyle :: forall a. Color -> Display -> Attribute a
cellStyle color display = style do
    C.display display
    C.position C.relative
    C.width boxSize
    C.height boxSize
    C.textAlign C.center
    C.fontSize (em 2.5)
    C.backgroundColor color
    C.padding (em 0.0) (em padding) (em 0.0) (em padding)
    apply4 C.margin (em 0.0)
    C.border C.solid (px 1.0) C.black
    where
      padding = 0.5
      boxSize = (em 1.5)

getJusts :: forall a. Array (Maybe a) -> Array a
getJusts = fromMaybe [] <<< sequence <<< Array.filter isJust

apply4 :: forall a b. (a -> a -> a -> a -> b) -> a -> b
apply4 f a = f a a a a
