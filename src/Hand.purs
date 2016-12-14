-- / What is in the hand during a round
module App.Hand where

import App.Board as Board
import App.Board (Board)
import App.BoardView as BoardView
import App.BoardView (class BoardView)
import Data.List (intercalate)
import Prelude (show, ($), (<>), map, (==))
import Pux.Html (Html, div, text)

newtype State = State
  { player :: Board.Player
  , seeds :: Board.Pit
  , pitRef :: Board.PitRef
  , board :: Board
  }

instance boardViewHand :: BoardView State where
  getBoard (State h) = h.board

  isPlaying (State h) player = h.player == player 

  pitState (State h) ref = 
    if ref == h.pitRef 
      then BoardView.Sowed
      else BoardView.Normal

init :: Board.Player -> Board.PitRef -> Board -> State
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