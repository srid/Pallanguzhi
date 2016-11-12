module Pallanguzhi.Msg exposing (..)

import Pallanguzhi.Board as Board

type Msg 
  = NoOp
  | Board Board.Msg
