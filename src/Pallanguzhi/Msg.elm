module Pallanguzhi.Msg exposing (..)

import Pallanguzhi.Board.Model as BoardModel

type Msg 
  = NoOp
  | Board BoardModel.Msg
