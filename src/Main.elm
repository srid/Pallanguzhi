import Html exposing (..)
import Html.App

import Dayakattai.Msg exposing (Msg)
import Dayakattai.Msg as Msg
import Dayakattai.View as View

import Dayakattai.Board as Board


-- Model

type alias Model =
  { board : Board.Model }

emptyModel : Model
emptyModel =
  { board = Board.emptyModel }

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

-- view

view : Model -> Html Msg
view model =
  View.viewBoard model.board

main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
