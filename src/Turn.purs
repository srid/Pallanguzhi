-- / A turn in a game round
module App.Turn where

import Data.Maybe
import App.Animation as Animation
import App.Board as Board
import App.Hand as Hand
import App.Animation (Animation)
import Data.List (List(..), (:), concat)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Prelude ((>>>), (<<<), ($), bind, pure)

type Turn =
  { hand :: Hand.State
  , board :: Board.State
  }

type State =
  { turn :: Turn
  , animation :: Animation Turn
  }

data Action = NextFrame

init :: Board.Player -> Board.State -> State
init player board = { turn, animation }
  where turn = { hand, board }
        hand = Hand.init player 0
        animation = unfoldTurns turn

update :: Action -> State -> Maybe State 
update NextFrame state = do
  Tuple turn' animation' <- Animation.step state.animation state.turn
  pure $ { turn: turn', animation: animation' }

turns :: Maybe Turn -> Maybe (Tuple (Animation Turn) (Maybe Turn))
turns Nothing = Nothing
turns (Just state@{ hand, board }) =
  Just t
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

unfoldTurns :: Turn -> Animation Turn
unfoldTurns = concat <<< unfoldr turns <<< Just

capture :: Turn -> Turn
capture state@{ hand, board } =
  state { board = board' }
  where seeds = Board.lookup hand.pitRef board
        board' = (Board.clear hand.pitRef >>> Board.store hand.player seeds) board

advance :: Turn -> Turn
advance x = x