module App.AI where

import App.Board as Board
import App.Hand as Hand
import App.Turn as Turn
import Data.Tuple as Tuple
import App.Board (Board, Player, PitRef)
import App.FixedMatrix72 (Ref(..), Row(..))
import Data.Array (filter, zipWith)
import Data.Foldable (maximum)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Ord, class Show, compare, eq, not, show, ($), (&&), (-), (<$>), (<<<), (<>), (==), (>))

newtype Selection = Selection (Tuple PitRef Int)

instance eqSelection :: Eq Selection where
  eq (Selection (Tuple r1 c1)) (Selection (Tuple r2 c2)) =
    eq c1 c2

instance ordSelection :: Ord Selection where
  compare (Selection (Tuple _ c1)) (Selection (Tuple _ c2)) = compare c1 c2

instance showSelection :: Show Selection where
  show (Selection (Tuple r1 c1)) =
    show "Selection: " <> show r1 <> " c=" <> show c1

selection :: PitRef -> Int -> Selection
selection ref = Selection <<< Tuple ref

suggest :: Player -> Board -> Maybe Selection
suggest player board = bestRef
  where bestRef = maximum $ zipWith selection refs (effectiveScore player <$> moves)
        moves = simulateMove player board <$> refs
        refs = filter validPit $ Board.refs player
        validPit r = Board.hasSeeds r board && not Board.isBlocked r board

simulateMove :: Player -> Board -> PitRef -> Board
simulateMove player board ref = Tuple.snd finalState
  where finalState = Turn.applyTurns (Turn.unfoldTurns state) state
        state = Tuple hand board
        hand = Hand.init player ref

effectiveScore :: Player -> Board -> Int
effectiveScore player board = playerStore - opponentStore
  where playerStore = Board.getStore player board
        opponentStore = Board.getStore (Board.opponentOf player) board
