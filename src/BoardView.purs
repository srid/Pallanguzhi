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
import Prelude (bind, const, pure, show, ($), (<), (<$>), (<<<), (<>), (==))
import Pux.CSS (Color, backgroundColor, em, hsl, lighten, padding, px, rotateHue, style)
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
pitColor (Just Turn.Sow) = lighten 0.3 $ pitColor Nothing
pitColor _ = hsl 70.0 1.0 0.3

view :: forall action state. BoardView state action
     => state -> Html action 
view state =
  div []
  [ viewStore state A
  , viewHandRow A
  , viewRow A
  , viewRow B
  , viewHandRow B
  , viewStore state B
  ]
  where
    board = 
      getBoard state
    viewHandRow player = 
      div [css] $ FM.mapRowWithIndex player (\ref pit -> viewHandInPit state ref) board.cells 
    viewRow player = 
      div [css] $ FM.mapRowWithIndex player (viewPit state) board.cells 
    css = style $ do 
            apply4 C.padding (em 0.5)

viewStore :: forall action state. BoardView state action
          => state -> Row -> Html action
viewStore state player = div [css] [text s] 
    where s = viewPlayer player
                <> " with "
                <> show seeds
                <> " seeds."
          seeds = getStore player board
          board = getBoard state
          css = style $ do 
            backgroundColor color 
            apply4 padding (em 0.5)
          color = if isPlaying state player 
                  then pitColor (Just Turn.Sow)
                  else pitColor Nothing 

viewPlayer :: Player -> String 
viewPlayer player = "Player " <> show player <> " " <> viewPlayerEmoji (Just player)

viewPlayerEmoji :: Maybe Player -> String 
viewPlayerEmoji (Just A) = "🐼"
viewPlayerEmoji (Just B) = "🐔"
viewPlayerEmoji Nothing = numberPadded 0

viewPitEmoji :: forall action state. BoardView state action 
              => state -> PitRef -> String 
viewPitEmoji state ref = viewPlayerEmoji $ do
  hand <- getHand state 
  if hand.pitRef == ref 
    then pure hand.player 
    else Nothing

viewPit :: forall action state. BoardView state action
        => state -> PitRef -> Pit -> Html action
viewPit state ref count =
  H.pre (getJusts [css, event]) [body]
  where
    body = text $ numberPadded count
    event = onClick <$> const <$> getPitAction state ref
    color = pitColor $ pitState state ref
    css = Just $ cellStyle color 

numberPadded :: Int -> String 
numberPadded n =
  if n < 10 
    then " " <> show n <> extra
    else show n <> extra
      where extra = "  "

cellStyle :: forall a. Color -> Attribute a
cellStyle color = style do
    C.display C.inline
    C.fontSize (em 2.0)
    C.backgroundColor color
    C.padding (em 0.0) (em padding) (em 0.0) (em padding)
    C.border C.solid (px 1.0) C.black
    where
      padding = 0.5

viewHandInPit :: forall action state. BoardView state action 
              => state -> PitRef -> Html action 
viewHandInPit state ref = 
  H.pre [cellStyle color] [ text s ] 
    where color = C.white
          s = fromMaybe (viewPlayerEmoji Nothing) do 
                hand <- getHand state   
                if hand.pitRef == ref 
                  then pure $ (viewPlayerEmoji $ Just hand.player) <> show hand.seeds 
                  else Nothing

getJusts :: forall a. Array (Maybe a) -> Array a 
getJusts = fromMaybe [] <<< sequence <<< Array.filter isJust  

apply4 :: forall a b. (a -> a -> a -> a -> b) -> a -> b
apply4 f a = f a a a a

