module App.Animation where

import Data.List (List, uncons)
import Data.Maybe (Maybe)
import Prelude (($), bind, pure)

type Frame a = (a -> a)

type Movie a = List (Frame a)

type State a =
  { current :: a
  , rest :: Movie a
  }

init :: forall a. a -> Movie a -> State a
init current rest = { current: current, rest: rest }

step :: forall a. State a -> Maybe (State a)
step { current, rest } = do
  { head: f, tail: rest' } <- uncons rest
  pure $ init (f current) rest'
