module App.View where 

import App.FixedMatrix72 as FM
import App.FixedMatrix72 (Row(B, A))
import App.Board (Pit, PitRef, Player, Board)
import Data.Maybe (Maybe(..))
import Prelude (bind, const, show, ($), (==), (<>))
import Pux.CSS (Color, backgroundColor, boxSizing, borderBox, display, em, inline, padding, rgb, style)
import Pux.Html (Html, div, hr, text, (#), (!))
import Pux.Html.Events (onClick)

class HasBoard a where
  getBoard :: a -> Board
  getBoardViewConfig :: a -> BoardViewConfig

newtype BoardViewConfig = BoardViewConfig
  { focusPit :: Maybe PitRef
  , focusPlayer :: Maybe Player
  }

viewBoard :: forall action state. HasBoard state 
          => (PitRef -> action) -> state -> Html action 
viewBoard f state =
  div []
  [ viewStore config A
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow A
  , hr [] [] -- XXX: remove this ugly display hack
  , div [] $ viewRow B
  , hr [] [] -- XXX: remove this ugly display hack
  , viewStore config B
  ]
  where
    viewRow player = FM.mapRowWithIndex player viewCell' board.cells
      where viewCell' ref = viewCell config (f ref) ref 
            board = getBoard state
    viewStore (BoardViewConfig c) player = 
      div ! css # text $ "Player " <> show player
        where css = style $ do 
                      backgroundColor color 
              color = if c.focusPlayer == Just player 
                      then focusColor 
                      else rgb 198 34 112
    config = getBoardViewConfig state

viewCell :: forall a. BoardViewConfig -> a -> PitRef -> Pit -> Html a
viewCell (BoardViewConfig config) action ref count =
  div ! css ! event # body
  where
    body = text $ show count
    event = onClick $ const action
    css = style $ do
      display inline
      backgroundColor color
      squarePadding (em 1.0)
      boxSizing borderBox
      where
        squarePadding sz = padding sz sz sz sz
        color = if config.focusPit == Just ref
                then focusColor
                else (rgb 200 100 0)

focusColor :: Color
focusColor = rgb 100 200 100