-- / A turn in a game round
module App.Turn where

import Data.Maybe
import App.Animation as Animation
import App.Board as Board
import App.Hand as Hand
import Data.List (List(..), (:), concat)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prelude ((>>>), (<<<), ($), map)

-- XXX: 'Turn' is a bad name; it is a verb but we need noun.
type Turn =
  { hand :: Hand.State
  , board :: Board.State
  }

type State = Animation.State Turn

data Action = NextFrame

init :: Board.Player -> Board.State -> State
init player board = Animation.init turn rest
  where turn = { hand, board }
        hand = Hand.init player 0
        rest = unfoldTurns turn

update :: Action -> State -> Maybe State
update NextFrame = Animation.step

-- TODO: fully implement and tidy up this function.
turns :: Turn -> Tuple (List (Turn -> Turn)) (Maybe Turn)
turns state@{ hand, board } =
  t
  where seedsBelow = Board.lookup hand.pitRef board
        nextRef = Board.nextRef hand.pitRef
        next2Ref = Board.nextRef nextRef
        seedsNext = Board.lookup nextRef board
        seedsNext2 = Board.lookup next2Ref board
        t = go hand.seeds seedsBelow seedsNext seedsNext2 state
        continue changes = Tuple changes (Just state) -- TODO apply them
        end changes = Tuple changes Nothing
        go 0 0 0 _ state' = end $ Nil
        go 0 0 s 0 state' =
          -- advance; capture; Nothing
          end $ advance : capture : Nil
        go _ _ _ _ state' = end $ Nil -- TODO

-- | A version of unfoldr that allows a elements in end cause.
unfoldr' :: forall a b t. Unfoldable t
        => (b -> Tuple a (Maybe b)) -> b -> t a
unfoldr' f = unfoldr (map f) <<< Just

unfoldTurns :: Turn -> List (Turn -> Turn)
unfoldTurns = concat <<< unfoldr' turns

capture :: Turn -> Turn
capture state@{ hand, board } =
  state { board = board' }
  where seeds = Board.lookup hand.pitRef board
        board' = (Board.clear hand.pitRef >>> Board.store hand.player seeds) board

advance :: Turn -> Turn
advance x = x
