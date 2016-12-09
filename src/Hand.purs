-- / What is in the hand during a round
module App.Hand where

import Data.Maybe (Maybe(..))
import App.Board as Board
import App.View (class HasBoard, ViewConfig(..))
import Pux.Html (Html, div, text)
import Prelude (show, ($), (<>))

newtype State = State
  { player :: Board.Player
  , seeds :: Board.Pit
  , pitRef :: Board.PitRef
  , board :: Board.State
  }

instance hasBoardHand :: HasBoard State where
  getBoard (State h) = 
    h.board
  getBoardViewConfig (State h) = 
    ViewConfig 
      { focusPit: Just h.pitRef 
      , focusPlayer: Just h.player 
      }

init :: Board.Player -> Board.PitRef -> Board.State -> State
init player pitRef board = State
  { player: player
  , seeds: 0
  , pitRef: pitRef
  , board: board
  }

opponent :: State -> Board.Player
opponent (State { player }) = Board.opponentOf player

view :: forall action. State -> Html action 
view (State h) =
  div []
    [ text "Hand by " 
    , text $ show h.player 
    , text $ " containing "
    , text $ show h.seeds
    , text " seeds at "
    , text $ show h.pitRef 
    , text $ " next3=" <> next3 
    ]
    where next3 = Board.mapPit3 h.pitRef (\s1 s2 s3 -> show s1 <> ":" <> show s2 <> ":" <> show s3) h.board