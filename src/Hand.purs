-- / What is in the hand during a round
module App.Hand where

import Data.Maybe (Maybe(..))
import App.Board as Board
import App.Board (class HasBoard)

newtype State = State
  { player :: Board.Player
  , seeds :: Board.Cell
  , pitRef :: Board.PitRef
  , board :: Board.State
  }

instance hasBoardHand :: HasBoard State where
  getBoard (State h) = 
    h.board
  getBoardViewConfig (State h) = 
    Board.ViewConfig 
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
