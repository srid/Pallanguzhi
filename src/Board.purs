module App.Board where

import Data.Foldable
import App.FixedMatrix72 as FM
import App.FixedMatrix72 (Ref(Ref), Row(..))
import Control.MonadZero (guard)
import Data.Array ((..), (:))
import Data.Maybe (Maybe(..))
import Prelude (bind, const, pure, (#), ($), (*), (+), (-), (<), (<#>), (<$>), (<<<), (==), (>), (>=))

type Board =
  { cells :: FM.FixedMatrix72 Pit
  , blockedCells :: Array PitRef
  , storeA :: Int
  , storeB :: Int
  }

type Pit = Int
type PitRef = FM.Ref
type Player = FM.Row

initPit :: Pit
initPit = 5

cols :: Int
cols = 7

rowCount :: Int
rowCount = cols * initPit

init' :: Int -> Board
init' perPit =
  { cells: FM.init perPit
  , blockedCells: []
  , storeA: 0
  , storeB: 0
  }

init :: Board
init = init' initPit

initWith' :: Int -> Int -> Int -> Board
initWith' a b perPit =
  init' 0
  # store A a
  # store B b
  # refillPlayer A
  # refillPlayer B
    where refillPlayer player board =
            foldl (refillPit player) board (refs player)
          refillPit player board ref =
            case unstoreToPit ref player perPit board of
              Just board' -> board'
              Nothing -> blockPit ref board

initWith :: Int -> Int -> Board
initWith a b | a < initPit  = initWith' a b 1
             | b < initPit  = initWith' a b 1
             | true         = initWith' a b 5

refs :: Player -> Array PitRef
refs A = FM.makeRef A <$> (cols-1)..0
refs B = FM.makeRef B <$> 0..(cols-1)

opponentOf :: Player -> Player
opponentOf A = B
opponentOf B = A

isBlocked :: PitRef -> Board -> Boolean
isBlocked ref = elem ref <<< _.blockedCells

hasSeeds :: PitRef -> Board -> Boolean
hasSeeds ref board = seeds > 0
  where seeds = lookup ref board

nextRef :: PitRef -> Board -> PitRef
nextRef ref board =
  if isBlocked next board
    then nextRef next board
    else next
    where next = nextRef' ref

nextRef' :: PitRef -> PitRef
nextRef' (Ref { row: A, idx: 0 })   = Ref { row: B, idx: 0 }
nextRef' (Ref { row: A, idx: idx }) = Ref { row: A, idx: idx - 1 }
nextRef' (Ref { row: B, idx: 6 })   = Ref { row: A, idx: 6 }
nextRef' (Ref { row: B, idx: idx }) = Ref { row: B, idx: idx + 1 }

blockPit :: PitRef -> Board -> Board
blockPit ref board = board { blockedCells = ref : board.blockedCells }

lookup :: PitRef -> Board -> Pit
lookup ref board = FM.lookup ref board.cells

mapPit :: forall a. PitRef -> (Pit -> a) -> Board -> a
mapPit ref f board = f $ lookup ref board

mapPit2 :: forall a. PitRef -> (Pit -> Pit -> a) -> Board -> a
mapPit2 ref1 f board = mapPit ref1 g board
  where g pit1 = mapPit (nextRef ref1 board) (f pit1) board

mapPit3 :: forall a. PitRef -> (Pit -> Pit -> Pit -> a) -> Board -> a
mapPit3 ref1 f board = mapPit ref1 g board
  where g pit1 = mapPit2 (nextRef ref1 board) (f pit1) board

belongsTo :: PitRef -> Player -> Boolean
belongsTo (Ref { row, idx }) player = row == player

modifyPit :: PitRef -> (Pit -> Pit) -> Board -> Board
modifyPit ref f board = board { cells = cells' }
   where cells' = FM.modify ref f board.cells

clearPit :: PitRef -> Board -> Board
clearPit pitRef = modifyPit pitRef (const 0)

setPit :: PitRef -> Int -> Board -> Board
setPit ref v = modifyPit ref (const v)

-- Move some seeds from a player's store to this pit
unstoreToPit :: PitRef -> Player -> Int -> Board -> Maybe Board
unstoreToPit ref player count board
    = unstore player count board
  <#> setPit ref count

-- Move all seeds from a pit to this player's store
storeFromPit :: Player -> Board -> PitRef -> Board
storeFromPit player board ref =
  board
  # clearPit ref
  # store player seeds
    where seeds = lookup ref board

store :: Player -> Int -> Board -> Board
store A seeds board = board { storeA = board.storeA + seeds }
store B seeds board = board { storeB = board.storeB + seeds }

unstore :: Player -> Int -> Board -> Maybe Board
unstore A count board = do
  guard $ board.storeA >= count
  pure $ board { storeA = board.storeA - count }
unstore B count board = do
  guard $ board.storeB >= count
  pure $ board { storeB = board.storeB - count }

getStore :: Player -> Board -> Pit
getStore A board = board.storeA
getStore B board = board.storeB
