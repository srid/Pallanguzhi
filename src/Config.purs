module App.Config where

import Prelude (($))
import Data.Maybe (Maybe(..))
import App.BoardView (class BoardView, getTurn)
import App.Turn (Turn(..))

type Config =
  { fastTurn :: Boolean
  }

turnDelay :: forall state action. BoardView state action
          => Config -> state -> Int
turnDelay config state =
  if config.fastTurn
    then 0
    else go $ getTurn state
    where go (Just Capture) = 500
          go (Just Lift) = 500
          go (Just Sow) = 50
          go _ = 100
