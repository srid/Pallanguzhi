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
import Data.Maybe (Maybe, fromJust, fromMaybe, isJust)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Monad, class Ord, class Show, bind, compare, eq, map, not, pure, show, ($), (&&), (-), (<$>), (<*>), (<<<), (<>), (==), (>))

newtype Selection = Selection (Tuple PitRef Board)

getRef :: Selection -> PitRef
getRef (Selection (Tuple r _)) = r

getPlayer :: Selection -> Player
getPlayer (Selection (Tuple (Ref { row, idx }) _)) = row

getScore :: Selection -> Int
getScore s = effectiveScore (getPlayer s) (getBoard s)

getOpponentScore :: Selection -> Int
getOpponentScore s = effectiveScore ((opponentOf <<< getPlayer) s) (getBoard s)

getBoard :: Selection -> Board
getBoard (Selection (Tuple _ b)) = b

instance showSelection :: Show Selection where
  show s = "Selection: " <> show (getRef s) <> " s=" <> show (getScore s)

selection :: PitRef -> Board -> Selection
selection ref = Selection <<< Tuple ref

suggest :: Boolean -> Player -> Board -> Maybe Selection
suggest shallow player board = bestSelection
  where bestSelection =
          maximumBy bestOf selections
        selections =
          zipWith selection refs $ getJusts finalBoards
        bestOf s1 s2 =
          compare (getScore s1) (getScore s2)
        finalBoards =
          case shallow of
            true  -> nextBoards
            false -> (map getBoard <<< suggest true (opponentOf player)) <$> getJusts nextBoards
        nextBoards =
          simulateMove player board <$> refs
        refs =
          filter (canLift board) $ Board.refs player

-- TODO: Move to util.purs
getJusts :: forall a. Array (Maybe a) -> Array a
getJusts = fromMaybe [] <<< sequence <<< filter isJust

-- TODO: move to Board.purs
canLift :: Board -> PitRef -> Boolean
canLift board r = Board.hasSeeds r board && not Board.isBlocked r board

simulateMove :: Player -> Board -> PitRef -> Maybe Board
simulateMove player board ref = do
  guard $ canLift board ref
  pure $ Tuple.snd $ Turn.run state
    where state = Tuple hand board
          hand = Hand.init player ref

effectiveScore :: Player -> Board -> Int
effectiveScore player board = playerStore - opponentStore
  where playerStore = Board.getStore player board
        opponentStore = Board.getStore (Board.opponentOf player) board
