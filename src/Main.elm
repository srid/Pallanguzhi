import Html exposing (..)
import Html.App

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Msg as Msg
import Pallanguzhi.View as View

import Pallanguzhi.Board as Board


-- Model

type alias Model =
  { board : Board.Board }

emptyModel : Model
emptyModel =
  { board = Board.initialBoard }

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

-- View

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
