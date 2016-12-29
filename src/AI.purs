module App.AI where

import App.Board as Board
import App.Hand as Hand
import App.Turn as Turn
import Data.Tuple as Tuple
import App.Board (Board, PitRef, Player, opponentOf)
import App.FixedMatrix72 (Ref(..), Row(..))
import Control.MonadZero (guard)
import Data.Array (filter, zipWith)
import Data.Foldable (maximum, maximumBy)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Monad, class Ord, class Show, bind, compare, const, eq, map, not, pure, show, ($), (&&), (-), (<$>), (<*>), (<<<), (<>), (==), (>))

-- TODO: Move to util.purs
getJusts :: forall a. Array (Maybe a) -> Array a
getJusts = fromMaybe [] <<< sequence <<< filter isJust

bestMove :: Board -> Player -> Maybe (Tuple PitRef Board)
bestMove board player =
  maximumBy bestOf $ zipWith Tuple possibleMoves nextBoards
    where nextBoards = simulateMove player board <$> possibleMoves
          possibleMoves = filter (canLift board) $ Board.refs player
          bestOf (Tuple _ b1) (Tuple _ b2) = on compare (effectiveScore player) b1 b2

-- TODO: move to Board.purs
canLift :: Board -> PitRef -> Boolean
canLift board r = Board.hasSeeds r board && not Board.isBlocked r board

simulateMove :: Player -> Board -> PitRef -> Board
simulateMove player board ref = Tuple.snd $ Turn.run state
    where state = Tuple hand board
          hand = Hand.init player ref

effectiveScore :: Player -> Board -> Int
effectiveScore player board = playerStore - opponentStore
  where playerStore = Board.getStore player board
        opponentStore = Board.getStore (Board.opponentOf player) board
