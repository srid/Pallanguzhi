module Pallanguzhi.Board exposing (..)

import List

type alias Board =
  { cups : List Cup
  }

type Cup = Cup Int

emptyBoard : Board
emptyBoard =
  { cups = List.repeat 14 emptyCup }

emptyCup : Cup
emptyCup = Cup 0

cupsA : Board -> List Cup
cupsA board =
  List.take 7 board.cups

cupsB : Board -> List Cup
cupsB board =
  List.drop 7 board.cups
