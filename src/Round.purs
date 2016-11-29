-- / Game round
module App.Round where

import App.Board as Board
import App.Hand as Hand

data State
  = Awaiting Hand.State Board.State
  | Completed Board.State

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