-- / A turn in a game round
module App.Turn where

import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import App.Animation (Animation)
import App.Board as Board
import App.Hand as Hand
import Data.List (List(..), (:), concat)
import Prelude ((>>>), (<<<), ($))

type Turn =
  { hand :: Hand.State
  , board :: Board.State
  }

type State =
  { turn :: Turn
  , animation :: Animation Turn
  }

init :: Board.Player -> Board.State -> State
init player board = { turn, animation }
  where turn = { hand, board }
        hand = Hand.init player 0
        animation = unfoldTurns turn

turns :: Maybe Turn -> Maybe (Tuple (Animation Turn) (Maybe Turn))
turns Nothing = Nothing
turns (Just state@{ hand, board }) =
  Just $ Tuple changes' (Just stateN)
  where seedsBelow = Board.lookup hand.pitRef board
        nextRef = Board.nextRef hand.pitRef
        next2Ref = Board.nextRef nextRef
        seedsNext = Board.lookup nextRef board
        seedsNext2 = Board.lookup next2Ref board
        changes' = go hand.seeds seedsBelow seedsNext seedsNext2 state 
        stateN = state --TODO
        go 0 0 0 _ state' = Nil 
        go 0 0 s 0 state' =
          -- advance; capture; Nothing
          advance : capture : Nil
        go _ _ _ _ state' = Nil -- TODO

unfoldTurns :: Turn -> Animation Turn
unfoldTurns = concat <<< unfoldr turns <<< Just

capture :: Turn -> Turn
capture state@{ hand, board } =
  state { board = board' }
  where seeds = Board.lookup hand.pitRef board
        board' = (Board.clear hand.pitRef >>> Board.store hand.player seeds) board

advance :: Turn -> Turn
advance x = x

