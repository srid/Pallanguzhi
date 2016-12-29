module App.AI where

import App.Board as Board
import App.Hand as Hand
import App.Turn as Turn
import Data.Tuple as Tuple
import App.Board (Board, PitRef, Player, opponentOf)
import App.FixedMatrix72 (Ref(..), Row(..), getValues)
import Control.MonadZero (guard)
import Data.Array (filter, zipWith)
import Data.Foldable (maximum, maximumBy, sum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Prelude (class Eq, class Monad, class Ord, class Show, bind, compare, const, eq, map, not, pure, show, ($), (&&), (+), (-), (/), (<$>), (<*>), (<<<), (<>), (==), (>))

type Move = Tuple PitRef Board

bestMove1 :: Board -> Player -> Maybe Move
bestMove1 board player =
  maximumBy bestOf $ possibleMoves board player
    where bestOf (Tuple _ b1) (Tuple _ b2) = on compare (effectiveScore player) b1 b2

-- Unlike bestMove1 consider opponent's best move as well.
bestMove2 :: Board -> Player -> Maybe Move
bestMove2 board player =
  fst <$> (maximumBy bestOf $ zipWith Tuple moves (calcScore <$> moves))
    where moves = possibleMoves board player
          calcScore m@(Tuple r b) = score b player (oppMove m)
          oppMove (Tuple r b) = bestMove1 b (opponentOf player)
          bestOf (Tuple _ s1) (Tuple _ s2) =
            compare s1 s2

possibleMoves :: Board -> Player -> Array Move
possibleMoves board player =
  zipWith Tuple possibleRefs nextBoards
    where nextBoards = simulateMove player board <$> possibleRefs
          possibleRefs = filter (canLift board) $ Board.refs player
          bestOf (Tuple _ b1) (Tuple _ b2) = on compare (effectiveScore player) b1 b2

score :: Board -> Player -> Maybe Move -> Int
score startBoard player Nothing =
  (sumInPit startBoard) + effectiveScore player startBoard
score startBoard player (Just (Tuple _ nextBoard)) =
  effectiveScore player nextBoard

-- Scoring algorithm. Pits in board is to be considered for balancing future moves.
effectiveScore :: Player -> Board -> Int
effectiveScore player board = playerStore + ((sumInPit board) / 2) -- opponentStore
  where playerStore = Board.getStore player board
        opponentStore = Board.getStore (Board.opponentOf player) board

sumInPit :: Board -> Int
sumInPit = sum <<< getValues <<< _.cells

canLift :: Board -> PitRef -> Boolean
canLift board r = Board.hasSeeds r board && not Board.isBlocked r board

simulateMove :: Player -> Board -> PitRef -> Board
simulateMove player board ref = Tuple.snd $ Turn.run state
    where state = Tuple hand board
          hand = Hand.init player ref

getJusts :: forall a. Array (Maybe a) -> Array a
getJusts = fromMaybe [] <<< sequence <<< filter isJust
