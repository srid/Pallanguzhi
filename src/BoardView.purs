module App.BoardView where

import Data.Maybe
import App.FixedMatrix72 as FM
import App.Turn as Turn
import Data.Array as Array
import Pux.CSS as C
import Pux.Html as H
import App.Board (Pit, PitRef, Player, Board, getStore, isBlocked)
import App.FixedMatrix72 (Row(B, A))
import App.Hand (Hand)
import App.Turn (Turn)
import CSS (gray)
import Control.MonadZero (guard)
import Data.Traversable (sequence)
import Data.Unfoldable (replicate)
import Prelude (bind, const, mod, pure, show, (#), ($), (-), (/), (<), (<$>), (<<<), (<=), (<>), (==))
import Pux.CSS (Color, em, pct, hsl, px, style)
import Pux.Html (Attribute, Html, div, text)
import Pux.Html.Events (onClick)

class BoardView state action | state -> action where
  getBoard :: state -> Board
  getHand :: state -> Maybe Hand
  getTurn :: state -> Maybe Turn
  getCurrentPlayer :: state -> Maybe Player
  getPitAction :: state -> PitRef -> Maybe action

type PitState = Maybe Turn

isPlaying :: forall state action. BoardView state action
          => state -> Player -> Boolean
isPlaying state player = fromMaybe false $ do
  currentPlayer <- getCurrentPlayer state
  pure $ player == currentPlayer

isCapturing :: forall state action. BoardView state action
            => state -> Player -> Boolean
isCapturing state player = fromMaybe false $ do
  guard $ isPlaying state player
  guard $ getTurn state == Just (Turn.Capture player)
  pure true

pitState :: forall state action. BoardView state action
         => state -> PitRef -> PitState
pitState state ref = do
  hand <- getHand state
  if ref == hand.pitRef
    then getTurn state
    else Nothing

playerColor :: Player -> Color
playerColor A = C.blue # C.lighten 0.2
playerColor B = C.red # C.lighten 0.2

playerColorFocus :: Player -> Color
playerColorFocus player = playerColor player # C.saturate 0.9

dynamicPlayerColor :: forall state action. BoardView state action
                   => state -> Player -> Color
dynamicPlayerColor state player = go (isPlaying state player) (isCapturing state player)
  where go _ true = playerColorFocus player
        go _ _ = playerColor player

pitColor :: PitState -> Color
pitColor (Just (Turn.Capture player)) = playerColorFocus player
pitColor (Just Turn.Lift) = pitColor Nothing # C.lighten 0.2
pitColor (Just Turn.Sow) = pitColor Nothing # C.lighten 0.4
pitColor _ = C.green

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
  H.pre [css] [ text s, viewSeeds 20 seeds ]
    where s = viewPlayer player <> showPadded seeds
          seeds = getStore player board
          board = getBoard state
          color = dynamicPlayerColor state player
          css = style do
            C.display C.inlineFlex
            C.textAlign C.center
            C.fontSize (em 0.8)
            C.backgroundColor color
            C.width (pct 40.0)
            C.height (em 6.0)
            C.marginLeft (pct 25.0)
            C.padding (em 0.0) (em padding) (em 0.0) (em padding)
            C.border C.dotted (px border) C.black
              where padding = 0.5
                    border = if isPlaying state player then 9.5 else 0.0


viewPit :: forall action state. BoardView state action
        => state -> PitRef -> Pit -> Html action
viewPit state ref count =
  H.div (getJusts [css, event]) [body]
  where
    body = div [] [content ]
    blocked = isBlocked ref (getBoard state)
    content = if blocked then text "X" else viewSeeds 5 count
    event = onClick <$> const <$> getPitAction state ref
    color = pitColor $ pitState state ref
    css = Just $ style do
      C.display C.inlineFlex
      C.width (pct 10.0)
      C.height (em 3.0)
      C.fontSize (em 2.5)
      C.backgroundColor $ if blocked then gray else color
      apply4 C.margin (em 0.0)
      C.border C.solid (px 1.0) C.black
      C.padding (em 0.0) (em padding) (em 0.0) (em padding)
        where padding = 0.5

viewSeed :: forall action. Html action
viewSeed = H.span [compactStyle] [ H.text $ "⦿"]

-- Display seeds as a matrix confined to a pit cell
viewSeeds :: forall action. Int -> Int -> Html action
viewSeeds g c | c == 0 = H.div [compactStyle] [ text "-"]
              | c <= g = H.div [compactStyle] $ replicate c viewSeed
              | true   = H.div [compactStyle] $ (viewSeeds g <$> splits)
                          where splits = replicate (c/g) g <> [c `mod` g]

compactStyle :: forall action. Attribute action
compactStyle = style do
  apply4 C.padding (px 0.0)
  apply4 C.margin (px 0.0)
  C.fontSize (px 24.0)
  C.width (em 1.0)
  C.height (em 1.0)

showPadded :: Int -> String
showPadded n =
  if n < 10
    then " " <> show n <> extra
    else show n <> extra
      where extra = "  "

viewPlayer :: Player -> String
viewPlayer A = "A"
viewPlayer B = "B"

getJusts :: forall a. Array (Maybe a) -> Array a
getJusts = fromMaybe [] <<< sequence <<< Array.filter isJust

apply4 :: forall a b. (a -> a -> a -> a -> b) -> a -> b
apply4 f a = f a a a a
