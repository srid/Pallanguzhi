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
        |> dig player loc
      else
        Err "Wrong player"
    (Continue, Seeding hand) ->
      model
      |> moveHand hand
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

moveHand : Hand -> Model -> Result String Model
moveHand hand model =
  let 
    lk loc = 
      model |> .board |> Board.lookup loc |> .seeds
    loc2 = 
      Board.next hand.loc
    loc3 = 
      Board.next loc2
  in
    case (hand.seeds, lk hand.loc, lk loc2) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        -- TODO: determine EndGame
        { model | state = Awaiting <| Board.otherPlayer hand.player }
        |> Ok
      (0, 0, s) -> -- Empty pit. Capture next and move on.
        model
        |> .board
        |> Board.clear loc2 
        |> Board.store hand.player s
        |> \board -> { model | board = board
                             , state = Seeding { hand | loc = loc3 }}
        |> Ok
      (0, s, _) -> -- Continue digging.
        model 
        |> .board
        |> Board.clear hand.loc
        |> \board -> { model | board = board
                             , state = Seeding { hand | seeds = s , loc = loc2 }}
        |> Ok
      (s, 5, _) -> -- Pasu; capture!
        model
        |> .board
        |> Board.clear hand.loc
        |> Board.store hand.player 6
        |> \board -> { model | board = board
                             , state = Seeding { hand | seeds = s - 1 , loc = loc2 }}
        |> Ok
      (s, _, _) -> -- Sow 1 seed and continue digging.
        model 
        |> .board
        |> Board.inc hand.loc
        |> \board -> { model | board = board
                             , state = Seeding { hand | seeds = s - 1 , loc = loc2 }}
        |> Ok

locFor : Board.Player -> Board.PitLocation -> Board.PitLocation
locFor player loc =
  case player of
    Board.A -> loc
    Board.B -> Board.pitsPerPlayer + loc

dig : Board.Player -> Board.PitLocation -> Model -> Result String Model
dig player loc model =
  newHand (locFor player loc) model
