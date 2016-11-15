module Pallanguzhi.Game.Model exposing (..)

import Time

import Return
import Return exposing (Return)

import Util.ElmExtra as E
import Pallanguzhi.Board.Model as Board

type Model
  = Awaiting Board.Player Board.Model
  | Seeding Hand Board.Model
  -- TODO: next rounds of game with rubbish holes respected
  | EndGame Board.Model

type alias Hand = 
  { player : Board.Player 
  , seeds : Int
  , loc : Board.PitLocation
  }

type Msg 
  = Reset
  | Play Board.Player Board.PitLocation
  | Continue

type alias Error = String

init : Model
init = Awaiting Board.A Board.init 

getBoard : Model -> Board.Model
getBoard model =
  case model of 
    Awaiting _ board -> board
    Seeding _ board -> board
    EndGame board -> board

transition : Msg -> Model -> Result Error Model
transition msg model = 
  case (msg, model) of
    (Reset, _) ->
      Ok init
    (Play player loc, Awaiting player_ board) ->
      if player == player_ then
        newHand (Board.locFor player loc) board
      else
        Err "Wrong player"
    (Continue, Seeding hand board) ->
      Ok <| moveHand hand board
    (_, _) ->
      Err "Bad game transition"

updateR : Msg -> Model -> Result Error (Return Msg Model)
updateR msg model =
  model 
  |> transition msg
  |> Result.map returnNext
                            
returnNext : Model -> Return Msg Model
returnNext model =
  case model of
    Seeding _ _ -> 
      E.sendAfter (Time.millisecond * 10) Continue
      |> Return.return model
    _ ->
      Return.singleton model

newHand : Board.PitLocation -> Board.Model -> Result String Model
newHand loc board =
  let 
    pit = 
      Board.lookup loc board
    hand =
      { player = pit.player
      , seeds = 0
      , loc = loc 
      }
  in 
    case pit.seeds of
      0 -> 
        Err "Nothing to lift from empty pit"
      _ ->
        Ok <| Seeding hand board

capture : (Hand, Board.Model) -> (Hand, Board.Model)
capture (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in
    ( hand
    , board
      |> Board.clear hand.loc
      |> Board.store hand.player seeds
    )

lift : (Hand, Board.Model) -> (Hand, Board.Model)
lift (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in 
    ( { hand | seeds = seeds }
    , Board.clear hand.loc board
    )

sow : (Hand, Board.Model) -> (Hand, Board.Model)
sow (hand, board) =
  ( { hand | seeds = hand.seeds - 1 }
  , Board.inc hand.loc board
  )

advance : (Hand, Board.Model) -> (Hand, Board.Model)
advance (hand, board) =
  let 
    newHand = { hand | loc = Board.next hand.loc }
  in
    (newHand, board)

moveHand : Hand -> Board.Model -> Model
moveHand hand board =
  let 
    seedsBelow =
      Board.lookupSeeds hand.loc board
    seedsNext =
      Board.lookupSeeds (Board.next hand.loc) board
    keepSeeding (hand, board) =
      Seeding hand board
    awaitForOtherPlayer (hand, board) =
      Awaiting (Board.otherPlayer hand.player) board
  in
    case (hand.seeds, seedsBelow, seedsNext) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        -- TODO: determine EndGame
        (hand, board)
        |> awaitForOtherPlayer
      (0, 0, s) -> -- Empty pit. Capture next and move on.
        (hand, board)
        |> advance 
        |> capture
        |> advance 
        |> keepSeeding
      (0, s, _) -> -- Continue digging.
        (hand, board)
        |> lift
        |> advance
        |> keepSeeding
      (s, 5, _) -> -- Pasu; capture!
        (hand, board)
        |> sow  -- Sow 1 before capturing the 6 seeds
        |> capture
        |> advance
        |> keepSeeding
      (s, _, _) -> -- Sow 1 seed and continue digging.
        (hand, board)
        |> sow
        |> advance
        |> keepSeeding
        