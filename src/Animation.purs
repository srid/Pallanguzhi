module App.Animation (Animation, Frame, step) where

import Data.List (List(..))
import Data.Tuple
import Data.Maybe (Maybe(..))
import Prelude (($))

type Frame state = (state -> state)

type Animation state = List (Frame state)

step :: forall a. Animation a 
               -> a 
               -> Maybe (Tuple a (Animation a))
step Nil state = 
  Nothing
step (Cons change movie') state = 
  Just $ Tuple (change state) movie'

{-doThis movie =
  case step movie 0 of 
    Nothing -> 
      finishTurn state'
    Just (Tuple state' movie') ->
      continueTurn state' movie'-}