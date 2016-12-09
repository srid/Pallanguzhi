-- / What is in the hand during a round
module App.Hand where

import App.Board as Board
import App.View (class HasBoard, ViewConfig(..))
import Data.List (intercalate)
import Data.Maybe (Maybe(..))
import Prelude (show, ($), (<>), map)
import Pux.Html (Html, div, text)

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
    [ text $ "Hand by " <> show h.player  
          <> " containing " <> show h.seeds
          <> " seeds at " <> show h.pitRef 
          <> " next3=" <> next3 
    ]
    where next3 = Board.mapPit3 h.pitRef 
                                (\s1 s2 s3 -> intercalate ":" $ map show [s1, s2, s3]) 
                                h.board