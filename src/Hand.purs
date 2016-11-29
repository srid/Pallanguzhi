-- / What is in the hand during a round
module App.Hand where 

import App.Board as Board 

type State = 
  { owner :: Board.Player 
  , seeds :: Board.Cell
  , pitRef :: Board.PitRef
  }

init :: Board.Player -> Board.Cell -> State 
init player seeds = 
  { owner: player
  , seeds: seeds 
  , pitRef: Board.makeRef player 0 
  }