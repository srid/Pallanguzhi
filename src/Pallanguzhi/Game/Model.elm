module Pallanguzhi.Game.Model exposing (..)

import Time

import Return
import Return exposing (Return)

import Util.ElmExtra as E
import Pallanguzhi.Board.Model as Board
import Pallanguzhi.Game.Hand exposing (Hand)
import Pallanguzhi.Game.Hand as Hand

-- TODO: model rounds
type Model
  = Awaiting Board.Player Board.Model
  | Seeding Hand Board.Model
  | EndGame Board.Model

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
      board
      |> Hand.new player_ (Board.locFor player loc) 
      |> Result.map (flip Seeding <| board)
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
      E.sendAfter (Time.millisecond * 5) Continue
      |> Return.return model
    _ ->
      Return.singleton model

moveHand : Hand -> Board.Model -> Model
moveHand hand board =
  let 
    opponent = Board.opponentOf hand.player
    (handMaybe, board_) = Hand.move hand board
  in
    case handMaybe of 
      Just hand_ ->
        Seeding hand_ board_
      Nothing ->
        if playerHasSeeds opponent board_ then
          Awaiting opponent board_
        else
          -- This should start next round.
          EndGame board_

playerHasSeeds : Board.Player -> Board.Model -> Bool
playerHasSeeds player board =
  board
  |> Board.rowOf player
  |> List.map .seeds
  |> List.sum
  |> ((<) 0)
