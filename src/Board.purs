module App.Board where

import App.FixedMatrix72 as FM
import App.FixedMatrix72 (Ref(Ref), Row(..))
import Prelude (const, ($), (+), (-), (==))

type Board =
  { cells :: FM.FixedMatrix72 Pit
  , storeA :: Int
  , storeB :: Int
  }

type Pit = Int
type PitRef = FM.Ref
type Player = FM.Row

init :: Board
init =
  { cells: FM.init 6
  , storeA: 0
  , storeB: 0
  }

opponentOf :: Player -> Player
opponentOf A = B
opponentOf B = A

nextRef :: PitRef -> PitRef
nextRef (Ref { row: A, idx: 0 })   = Ref { row: B, idx: 0 }
nextRef (Ref { row: A, idx: idx }) = Ref { row: A, idx: idx - 1 }
nextRef (Ref { row: B, idx: 6 })   = Ref { row: A, idx: 6 }
nextRef (Ref { row: B, idx: idx }) = Ref { row: B, idx: idx + 1 }

lookup :: PitRef -> Board -> Pit
lookup ref board = FM.lookup ref board.cells

mapPit :: forall a. PitRef -> (Pit -> a) -> Board -> a
mapPit ref f board = f $ lookup ref board

mapPit2 :: forall a. PitRef -> (Pit -> Pit -> a) -> Board -> a
mapPit2 ref1 f board = mapPit ref1 g board
  where g pit1 = mapPit (nextRef ref1) (f pit1) board

mapPit3 :: forall a. PitRef -> (Pit -> Pit -> Pit -> a) -> Board -> a
mapPit3 ref1 f board = mapPit ref1 g board
  where g pit1 = mapPit2 (nextRef ref1) (f pit1) board

belongsTo :: PitRef -> Player -> Boolean 
belongsTo (Ref { row, idx }) player = row == player

modify :: PitRef -> (Pit -> Pit) -> Board -> Board
modify ref f board = board { cells = cells' }
   where cells' = FM.modify ref f board.cells

clear :: PitRef -> Board -> Board
clear pitRef = modify pitRef (const 0)

store :: Player -> Pit -> Board -> Board
store A seeds board = board { storeA = board.storeA + seeds }
store B seeds board = board { storeA = board.storeB + seeds }

getStore :: Player -> Board -> Pit 
getStore A board = board.storeA
getStore B board = board.storeB