import Html exposing (..)
import Html.App

-- Model

type alias Model = Int

init : (Model, Cmd Msg)
init =
  (1, Cmd.none)

-- Update

type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

-- view

view : Model -> Html Msg
view model =
  text "Hello"

main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
