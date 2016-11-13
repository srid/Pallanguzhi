import Html exposing (..)
import Html.App
import Return
import Return exposing (Return)

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Msg as Msg
import Pallanguzhi.View as View
import Pallanguzhi.Board as Board

import Util.ModelE as ModelE

-- Model

type alias Model =
  { board : ModelE.ModelE String Board.Model 
  }

init : Return Msg Model
init = { board = Ok Board.init}
       |> Return.singleton

-- Update

update : Msg -> Model -> Return Msg Model
update msg model =
  case msg of
    Msg.NoOp ->
      Return.singleton model
    Msg.Board action ->
      model.board
      |> (ModelE.update Board.updateR) action
      |> Return.mapBoth Msg.Board (\b -> {model | board = b})

-- View

view : Model -> Html Msg
view model =
  (ModelE.view View.viewBoard) model.board


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
