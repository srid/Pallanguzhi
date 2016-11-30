module App.Animation (Animation, Frame, step) where

import Data.Tuple
import Data.List (List, uncons)
import Data.Maybe (Maybe)
import Prelude (($), bind, pure)

type Frame state = (state -> state)

type Animation state = List (Frame state)

-- TODO: perhaps abstract the Animation a tuple and include an update function.
step :: forall a. Animation a 
               -> a 
               -> Maybe (Tuple a (Animation a))
step animation state = do
  { head: f, tail: animation' } <- uncons animation
  pure $ Tuple (f state) animation'
  
{-doThis movie =
  case step movie 0 of 
    Nothing -> 
      finishTurn state'
    Just (Tuple state' movie') ->
      continueTurn state' movie'-}

