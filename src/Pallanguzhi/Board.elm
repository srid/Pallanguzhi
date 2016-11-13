module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type Player = A | B 
type alias PitLocation = Int
type alias Pit = { player : Player, seeds : Int}

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


pitsPerPlayer : number
pitsPerPlayer = 7

seedsPerPit : number
seedsPerPit = 12

initialModel : Model
initialModel = 
  let 
    s = 
      seedsPerPit
    row = 
      [s, s, s, 2, s, s, s] |> Array.fromList
    makePit player seeds =
      {player = player, seeds = seeds}
    makeRow player = 
      row
      |> Array.map (makePit player) 
  in
    { pits = makeRow A `Array.append` makeRow B
    , storeA = 0
    , storeB = 0
    }

rows : Model -> (List Pit, List Pit)
rows model = 
  let 
    f g = Array.toList >> g pitsPerPlayer
  in 
    ( f List.take model.pits
    , f List.drop model.pits
    )

lookup : PitLocation -> Model -> Pit
lookup loc model = 
  case Array.get loc model.pits of
    Just pit -> 
      pit
    Nothing  -> 
      -- Invalid index is only possible due to programmer error.
      Debug.crash <| "error: invalid index: " ++ (toString loc)

next : PitLocation -> PitLocation
next loc =
  let 
    total = 2 * pitsPerPlayer 
  in 
    (loc + 1) % total

updateSeeds : PitLocation -> (Int -> Int) -> Model -> Model
updateSeeds loc f model =
  let 
    pit = 
      lookup loc model
    pits = 
      Array.set loc { pit | seeds = f pit.seeds } model.pits
  in
    { model | pits = pits }

inc : PitLocation -> Model -> Model
inc loc model =
  updateSeeds loc (\s -> s + 1)  model

clear : PitLocation -> Model -> Model
clear loc model =
  updateSeeds loc (always 0) model

store : Player -> Int -> Model -> Model
store player seeds model =
  case player of
    A -> { model | storeA = model.storeA + seeds }
    B -> { model | storeB = model.storeB + seeds }

capture : Player -> PitLocation -> Model -> Model
capture player loc model = 
  let 
    c = lookup loc model |> .seeds
  in
    model |> clear loc |> store player c 

dig : PitLocation -> Model -> Model
dig loc model =
  let 
    pit = lookup loc model
  in
    digAs pit.player loc model

digAs : Player -> PitLocation -> Model -> Model
digAs player loc model =
  let
    hand = lookup loc model |> .seeds
  in
    model 
    |> clear loc 
    |> sowAs player hand (next loc)

sowAs : Player -> Int -> PitLocation -> Model -> Model
sowAs player hand loc1 model =
  let 
    lk loc = 
      model |> lookup loc |> .seeds
    loc2 = 
      next loc1
    loc3 = 
      next loc2
  in
    model
    |> case (hand, lk loc1, lk loc2) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        identity
      (0, 0, _) -> -- Empty pit. Capture next and move on.
        capture player loc2
        >> digAs player loc3
      (0, _, _) -> -- Continue digging.
        digAs player loc1
      _ -> -- Sow 1 seed and continue digging.
        inc loc1
        >> sowAs player (hand-1) loc2