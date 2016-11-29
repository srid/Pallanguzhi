-- / Game round
module App.Round where

import Prelude ((>>>))
import App.Board as Board
import App.Hand as Hand

data State
  = Awaiting Hand.State Board.State
  | Completed Board.State

-- This should be modeled as animation frame change
type Change = State -> State

init :: Board.Player -> Board.State -> State 
init player = Awaiting hand 
  where hand = Hand.init player 0

turn :: State -> State 
turn (Completed board) = 
  Completed board -- Not possible
turn (Awaiting hand board) =
  Completed board -- TODO
  where seedsBelow' = 0
        nextRef = Board.nextRef hand.pitRef
        next2Ref = Board.nextRef nextRef
        seedsNext = Board.lookup nextRef board

capture :: Change
capture (Completed board) =
  Completed board -- XXX: Not possible
capture (Awaiting hand board) =
  Awaiting hand board' 
  where seeds = Board.lookup hand.pitRef board
        board' = (Board.clear hand.pitRef >>> Board.store hand.owner seeds) board


