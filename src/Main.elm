import Html exposing (..)
import Html.App

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Msg as Msg
import Pallanguzhi.View as View
import Pallanguzhi.Board as Board

import Util.ModelE as ModelE

-- Model

type alias Model =
  { board : ModelE.ModelE String Board.Model }

emptyModel : Model
emptyModel =
  { board = Ok Board.initialModel }

init : (Model, Cmd Msg)
init =
  ( emptyModel
  , Cmd.none)

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Msg.NoOp ->
      (model, Cmd.none)
    Msg.Board action ->
      let 
        (model', cmd') = (ModelE.update Board.updateR) action model.board
      in
        ({model | board = model'}, Cmd.map Msg.Board cmd')

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
