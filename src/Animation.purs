module App.Animation where

import Data.List (List, uncons)
import Data.Maybe (Maybe(..))
import Prelude (($), bind, pure)

class Transition f a where 
  getTransitionF :: f a -> (a -> a)

type State f a =
  { current :: a
  , lastTransition :: Maybe (f a)
  , rest :: List (f a)
  }

init :: forall f a. a -> List (f a) -> State f a
init = { current: _
       , lastTransition: Nothing
       , rest: _ }

step :: forall f a. Transition f a => State f a -> Maybe (State f a)
step { current, rest } = do
  { head: t, tail: rest' } <- uncons rest
  pure $ { current: (getTransitionF t) current 
         , lastTransition: Just t 
         , rest: rest' 
         }