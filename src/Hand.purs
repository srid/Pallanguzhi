-- / What is in the hand during a round
module App.Hand where

import App.Board (Player, Pit, PitRef, opponentOf)

type Hand = 
  { player :: Player
  , seeds :: Pit
  , pitRef :: PitRef
  }

init :: Player -> PitRef -> Hand
init player pitRef = 
  { player: player
  , seeds: 0
  , pitRef: pitRef
  }

opponent :: Hand -> Player
opponent hand = opponentOf hand.player
