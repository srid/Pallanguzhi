module App.Board where

import App.FixedMatrix72 as FM
import App.FixedMatrix72 (Ref(Ref), Row(..))
import Prelude (const, (==), ($), (+), (-), (<<<))

-- TODO: rename to Pit
type Pit = Int

type State =
  { cells :: FM.FixedMatrix72 Pit
  , storeA :: Int
  , storeB :: Int
  }

type Player = FM.Row

type PitRef = Ref
makeRef :: Player -> Int -> PitRef
makeRef = FM.makeRef

nextRef :: PitRef -> PitRef
nextRef (Ref { row: A, idx: 0 })   = Ref { row: B, idx: 0 }
nextRef (Ref { row: A, idx: idx }) = Ref { row: A, idx: idx - 1 }
nextRef (Ref { row: B, idx: 6 })   = Ref { row: A, idx: 6 }
nextRef (Ref { row: B, idx: idx }) = Ref { row: B, idx: idx + 1 }

lookup :: PitRef -> State -> Pit
lookup ref board = FM.lookup ref board.cells

belongsTo :: PitRef -> Player -> Boolean 
belongsTo (Ref { row, idx }) player = row == player

mapPit :: forall a. PitRef -> (Pit -> a) -> State -> a
mapPit ref f board = f $ lookup ref board

mapPit2 :: forall a. PitRef -> (Pit -> Pit -> a) -> State -> a
mapPit2 ref1 f board = mapPit ref1 g board
  where g pit1 = mapPit (nextRef ref1) (f pit1) board

mapPit3 :: forall a. PitRef -> (Pit -> Pit -> Pit -> a) -> State -> a
mapPit3 ref1 f board = mapPit ref1 g board
  where g pit1 = mapPit2 (nextRef ref1) (f pit1) board

playerCells :: Player -> State -> Array Pit
playerCells player = FM.getRow player <<< _.cells

opponentOf :: Player -> Player
opponentOf A = B
opponentOf B = A

init :: State
init =
  { cells: FM.init 6
  , storeA: 0
  , storeB: 0
  }

modify :: PitRef -> (Pit -> Pit) -> State -> State
modify ref f board = board { cells = cells' }
   where cells' = FM.modify ref f board.cells

clear :: PitRef -> State -> State
clear pitRef = modify pitRef (const 0)

store :: Player -> Pit -> State -> State
store A seeds state = state { storeA = state.storeA + seeds }
store B seeds state = state { storeA = state.storeB + seeds }