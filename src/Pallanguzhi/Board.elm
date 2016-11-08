module Pallanguzhi.Board exposing (..)

type alias Model =
  { playerA : Player
  , playerB : Player }

emptyModel : Model
emptyModel =
  { playerA = emptyPlayer
  , playerB = emptyPlayer }

type alias Player =
  { chips : List Chip }

emptyPlayer : Player
emptyPlayer =
  let totalChips = 6
  in  { chips = List.repeat totalChips emptyChip }

type alias Chip =
  { location : Location }

emptyChip : Chip
emptyChip =
  { location = emptyLocation }

type Location = Location Int

emptyLocation : Location
emptyLocation = Location 0
