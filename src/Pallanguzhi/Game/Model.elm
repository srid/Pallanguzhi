module Pallanguzhi.Game.Model exposing (..)

import Time

import Return
import Return exposing (Return)

import Util.ElmExtra as E
import Pallanguzhi.Board.Model as Board

type State
  = Awaiting Board.Player
  | Seeding Hand
  -- TODO: next rounds of game with rubbish holes respected
  | EndGame

type alias Model =
  { board : Board.Model
  , state : State
  }
  
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
init = { board = Board.init
       , state = Awaiting Board.A
       }

transition : Msg -> Model -> Result Error Model
transition msg model = 
  case (msg, model.state) of
    (Reset, _) ->
      Ok init
    (Play player loc, Awaiting player_) ->
      if player == player_ then
        model
        |> newHand (Board.locFor player loc) 
      else
        Err "Wrong player"
    (Continue, Seeding hand) ->
      model
      |> moveHand hand
      |> Ok
    (_, _) ->
      Err "Bad game transition"

updateR : Msg -> Model -> Result Error (Return Msg Model)
updateR msg model =
  model 
  |> transition msg
  |> Result.map returnNext
                            
returnNext : Model -> Return Msg Model
returnNext model =
  case model.state of
    Seeding _ -> 
      E.sendAfter (Time.millisecond * 10) Continue
      |> Return.return model
    _ ->
      Return.singleton model

newHand : Board.PitLocation -> Model -> Result String Model
newHand loc model =
  let 
    pit = 
      Board.lookup loc model.board
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
        Ok { model | state = Seeding hand }

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

moveHand : Hand -> Model -> Model
moveHand hand model =
  let 
    seedsBelow =
      Board.lookupSeeds hand.loc model.board
    seedsNext =
      Board.lookupSeeds (Board.next hand.loc) model.board
    keepSeeding (hand, board) =
      { model | board = board, state = Seeding hand }
    awaitForOtherPlayer (hand, board) =
      { model | state = Awaiting <| Board.otherPlayer hand.player }
  in
    case (hand.seeds, seedsBelow, seedsNext) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        -- TODO: determine EndGame
        (hand, model.board)
        |> awaitForOtherPlayer
      (0, 0, s) -> -- Empty pit. Capture next and move on.
        (hand, model.board)
        |> advance 
        |> capture
        |> advance 
        |> keepSeeding
      (0, s, _) -> -- Continue digging.
        (hand, model.board)
        |> lift
        |> advance
        |> keepSeeding
      (s, 5, _) -> -- Pasu; capture!
        (hand, model.board)
        |> sow  -- Sow 1 before capturing the 6 seeds
        |> capture
        |> advance
        |> keepSeeding
      (s, _, _) -> -- Sow 1 seed and continue digging.
        (hand, model.board)
        |> sow
        |> advance
        |> keepSeeding
        