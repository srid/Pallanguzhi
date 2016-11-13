module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type Player = A | B 
type alias PitLocation = Int
type alias Pit = { player : Player, seeds : Int}

type alias Model =
  { pits : Array Pit
  , storeA : Int
  , storeB : Int
  , hand : Maybe Hand
  , error : Maybe String -- XXX: is this right abstraction?
  }
  
type alias Hand = 
  { player : Player 
  , seeds : Int
  , loc : PitLocation
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
      (dig pitLoc model |> resultToError model, Cmd.none)

resultToError : Model -> Result String Model -> Model 
resultToError m r =
  case r of
    Ok m' -> { m' | error = Nothing }
    Err e -> { m | error = Just e }

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
    , hand = Nothing
    , error = Nothing
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

-- 

newHand : PitLocation -> Model -> Result String Model
newHand loc model =
  case model.hand of
    Just _ -> 
      Err "already has a hand"
    Nothing ->
      let 
        pit = 
          lookup loc model
        hand =
          { player = pit.player
          , seeds = 0
          , loc = loc 
          }
      in 
        case pit.seeds of
          0 -> 
            Err "cannot select empty pit"
          _ ->
            Ok { model | hand = Just hand }

moveHand : Model -> Result String Model
moveHand model =
  case model.hand of 
    Nothing -> 
      Err "no hand"
    Just hand ->
      let 
        lk loc = 
          model |> lookup loc |> .seeds
        loc2 = 
          next hand.loc
        loc3 = 
          next loc2
      in
        case (hand.seeds, lk hand.loc, lk loc2) of
          (0, 0, 0) -> -- No hand, next two pits empty. End turn.
            { model | hand = Nothing }
            |> Ok
          (0, 0, s) -> -- Empty pit. Capture next and move on.
            model
            |> clear loc2 
            |> store hand.player s
            |> \model -> { model | hand = Just { hand | loc = loc3 }}
            |> Ok
          (0, s, _) -> -- Continue digging.
            model 
            |> clear hand.loc
            |> \model -> { model | hand = Just { hand | seeds = s, loc = loc2 }}
            |> Ok
          (s, _, _) -> -- Sow 1 seed and continue digging.
            model 
            |> inc hand.loc
            |> \model -> { model | hand = Just { hand | seeds = s - 1, loc = loc2 }}
            |> Ok

runHand : Model -> Result String Model
runHand model = 
  moveHand model 
  `Result.andThen` (\model -> case model.hand of
                                Nothing -> Ok model
                                _       -> runHand model)

dig : PitLocation -> Model -> Result String Model
dig loc model =
  newHand loc model
  `Result.andThen` runHand
