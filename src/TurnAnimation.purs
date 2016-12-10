module App.TurnAnimation where

import Data.List (List, uncons)
import Data.Maybe (Maybe(..))
import Prelude (($), bind, pure)
import Pux (EffModel)
import Control.Monad.Aff (later')

class Turnable model turn where
  runTurn :: turn model -> model -> model
  turnDelay :: Maybe (turn model) -> Int

type State model turn = 
  { current :: model 
  , lastTurn :: Maybe (turn model)
  , remainingTurns :: List (turn model)
  }

data Action = NextFrame

init :: forall model turn. model -> List (turn model) -> State model turn 
init = { current: _, lastTurn: Nothing, remainingTurns: _ }

update :: forall model turn eff. 
          Turnable model turn
       => Action 
       -> State model turn 
       -> Maybe (EffModel (State model turn) Action (eff))
update NextFrame state = do
  state' <- nextFrame state 
  pure $ withAnimateEffect state'

withAnimateEffect :: forall model turn eff.
                     Turnable model turn
                  => State model turn 
                  -> EffModel (State model turn) Action (eff)
withAnimateEffect state = 
  { state: state 
  , effects: [ later' (turnDelay state.lastTurn) (pure NextFrame) ]
  }

nextFrame :: forall model turn. 
             Turnable model turn 
          => State model turn 
          -> Maybe (State model turn)
nextFrame { current, lastTurn, remainingTurns } = do
  { head: currentTurn, tail: remainingTurns' } <- uncons remainingTurns
  pure { current: runTurn currentTurn current 
       , lastTurn: Just currentTurn 
       , remainingTurns: remainingTurns' 
       }