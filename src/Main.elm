import Html exposing (..)
import Html.App
import Return
import Return exposing (Return)

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Msg as Msg
import Pallanguzhi.BoardE as BoardE


-- Model

type alias Model =
  { board : BoardE.Model
  }

init : Return Msg Model
init = { board = BoardE.init }
       |> Return.singleton

-- Update

update : Msg -> Model -> Return Msg Model
update msg model =
  case msg of
    Msg.NoOp ->
      Return.singleton model
    Msg.Board action ->
      model.board
      |> BoardE.update action
      |> Return.mapBoth Msg.Board (\b -> {model | board = b})

-- View

view : Model -> Html Msg
view model =
  BoardE.view model.board


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
