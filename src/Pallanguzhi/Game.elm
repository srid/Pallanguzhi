module Pallanguzhi.Game exposing (..)

import Time

import Return
import Return exposing (Return)

import Util.ElmExtra as E
import Pallanguzhi.Board exposing (Board)
import Pallanguzhi.Board as Board
import Pallanguzhi.Hand exposing (Hand)
import Pallanguzhi.Hand as Hand

-- TODO: model rounds
type Model
  = Awaiting Board.Player Board
  | Seeding Hand Board
  | EndGame Board

type Msg 
  = Reset
  | Play Board.PitLocation
  | Continue

type alias Error = String

init : Model
init = Awaiting Board.A Board.init 

getBoard : Model -> Board
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
    (Play loc, Awaiting player_ board) ->
      board
      |> Hand.new player_ loc
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
      E.sendAfter (Time.millisecond * 300) Continue
      |> Return.return model
    _ ->
      Return.singleton model

moveHand : Hand -> Board -> Model
moveHand hand board =
  let 
    opponent = Board.opponentOf hand.player
    (hand_, board_) = Hand.move (hand, board)
  in
    if Hand.shouldEndTurn hand_ then
      if playerHasSeeds opponent board_ then
        Awaiting opponent board_
      else
        EndGame board_
    else
      Seeding hand_ board_

playerHasSeeds : Board.Player -> Board -> Bool
playerHasSeeds player board =
  board
  |> Board.rowOf player
  |> List.map .seeds
  |> List.sum
  |> ((<) 0)
