-- / What is in the hand during a round
module App.Hand where 

import App.Board as Board 

type State = 
  { player :: Board.Player 
  , seeds :: Board.Cell
  , pitRef :: Board.PitRef
  }

init :: Board.Player -> Board.Cell -> State 
init player seeds = 
  { player: player
  , seeds: seeds 
  , pitRef: Board.makeRef player 0 
  }