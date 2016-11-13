module Pallanguzhi.Game.Model exposing (..)

import Return
import Return exposing (Return)

import Pallanguzhi.Board.Model as Board

type alias Model =
  { board : Board.Model
  , hand : Maybe Hand  
  }
  
type alias Hand = 
  { player : Board.Player 
  , seeds : Int
  , loc : Board.PitLocation
  }

type Msg 
  = Reset
  | Play Board.Player Board.PitLocation

init : Model
init = { board = Board.init
       , hand = Nothing
       }

updateR : Msg -> Model -> Result String (Return Msg Model)
updateR msg model =
  (case msg of
    Reset ->
      init
      |> Ok
    Play player pitLoc ->
      model
      |> dig player pitLoc)
  |> Result.map Return.singleton
  
withHand : Model -> (Hand -> Result String Model) -> Result String Model
withHand model f =
  case model.hand of 
    Just hand ->
      f hand
    Nothing ->
      Err "no hand"

withoutHand : Model -> (Model -> Result String Model) -> Result String Model
withoutHand model f =
  case model.hand of
    Just _ ->
      Err "already has a hand"
    Nothing ->
      f model

newHand : Board.PitLocation -> Model -> Result String Model
newHand loc model =
  withoutHand model <| \model ->
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
          Err "cannot select empty pit"
        _ ->
          Ok { model | hand = Just hand }

moveHand : Model -> Result String Model
moveHand model =
  withHand model <| \hand ->
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
          { model | hand = Nothing }
          |> Ok
        (0, 0, s) -> -- Empty pit. Capture next and move on.
          model
          |> .board
          |> Board.clear loc2 
          |> Board.store hand.player s
          |> \board -> { model | board = board
                               , hand = Just { hand | loc = loc3 }}
          |> Ok
        (0, s, _) -> -- Continue digging.
          model 
          |> .board
          |> Board.clear hand.loc
          |> \board -> { model | board = board
                               , hand = Just { hand | seeds = s
                                                    , loc = loc2 }}
          |> Ok
        (s, _, _) -> -- Sow 1 seed and continue digging.
          model 
          |> .board
          |> Board.inc hand.loc
          |> \board -> { model | board = board
                               , hand = Just { hand | seeds = s - 1
                                                    , loc = loc2 }}
          |> Ok

runHand : Model -> Result String Model
runHand model = 
  moveHand model 
  `Result.andThen` (\model -> case model.hand of
                                Nothing -> Ok model
                                _       -> runHand model)

locFor : Board.Player -> Board.PitLocation -> Board.PitLocation
locFor player loc =
  case player of
    Board.A -> loc
    Board.B -> Board.pitsPerPlayer + loc

dig : Board.Player -> Board.PitLocation -> Model -> Result String Model
dig player loc model =
  newHand (locFor player loc) model
  `Result.andThen` runHand
