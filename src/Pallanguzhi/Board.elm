module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type alias PitLocation = Int

type alias Model =
  { pits   : Array Pit
  , storeA : Int
  , storeB : Int
  }

type Msg 
  = Reset
  | Play Player PitLocation

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      (initialModel, Cmd.none)
    Play player pitLoc ->
      (digAs player pitLoc model, Cmd.none)

type Player = A | B 
type alias Pit = { player : Player, seeds : Int}

pitsPerPlayer : number
pitsPerPlayer = 7

seedsPerPit : number
seedsPerPit = 12

initialModel : Model
initialModel = 
  let 
    makeRow player = Array.repeat pitsPerPlayer {player = player, seeds = seedsPerPit}
  in
    { pits = makeRow A `Array.append` makeRow B
    , storeA = 0
    , storeB = 0
    }
    |> dig 0

rows : Model -> (List Pit, List Pit)
rows model = 
  let 
    f g = Array.toList >> g pitsPerPlayer
  in 
    ( f List.take model.pits
    , f List.drop model.pits
    )

lookup : Int -> Model -> Pit
lookup idx model = 
  case Array.get idx model.pits of
    Just pit -> 
      pit
    Nothing  -> 
      -- Invalid index is only possible due to programmer error.
      Debug.crash <| "error: invalid index: " ++ (toString idx)

next : Int -> Int
next idx
  = let 
      total = 2 * pitsPerPlayer 
    in 
      (idx + 1) % total

updateSeeds : Int -> (Int -> Int) -> Model -> Model
updateSeeds idx f model =
  let 
    pit = lookup idx model
    pits = Array.set idx { pit | seeds = f pit.seeds } model.pits
  in
    { model | pits = pits }

inc : Int -> Model -> Model
inc idx model =
  updateSeeds idx (\s -> s + 1)  model

clear : Int -> Model -> Model
clear idx model =
  updateSeeds idx (always 0) model

store : Player -> Int -> Model -> Model
store player seeds model =
  case player of
    A -> { model | storeA = model.storeA + seeds }
    B -> { model | storeA = model.storeB + seeds }

capture : Player -> Int -> Model -> Model
capture player idx model = 
  let 
    c = lookup idx model |> .seeds
  in
    model |> clear idx |> store player c 

dig : Int -> Model -> Model
dig idx model =
  let 
    pit = lookup idx model
  in
    digAs pit.player idx model

digAs : Player -> Int -> Model -> Model
digAs player idx model =
  let
    hand = lookup idx model |> .seeds
  in
    model 
    |> clear idx 
    |> sowAs player hand (next idx)

sowAs : Player -> Int -> Int -> Model -> Model
sowAs player hand idx model =
  let 
    lk idx = model |> lookup idx |> .seeds
    idxN   = next idx
    idxNN  = next idxN
  in
    model
    |> case (hand, lk idx, lk idxN) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        identity
      (0, 0, _) -> -- Empty pit. Capture next and move on.
        capture player idxN
        >> digAs player idxNN
      (0, _, _) -> -- Continue digging.
        digAs player idx
      _ -> -- Sow 1 seed and continue digging.
        inc idx
        >> sowAs player (hand-1) idxN