import Html exposing (..)
import Html.App
import Return
import Return exposing (Return)

import Pallanguzhi.Game.Model as Game
import Pallanguzhi.Game.View as GameView

import Util.ModelE exposing (ModelES)
import Util.ModelE as ModelE

-- Model

type alias Model =
  { game : ModelES Game.Model
  }

init : Return Msg Model
init = { game = ModelE.init Game.init }
       |> Return.singleton

-- Msg
      
type Msg 
  = NoOp
  | GameMsg Game.Msg

-- Update

update : Msg -> Model -> Return Msg Model
update msg model =
  case msg of
    NoOp ->
      Return.singleton model
    GameMsg action ->
      model
      |> .game
      |> (ModelE.update Game.updateR) action
      |> Return.mapBoth GameMsg (\g -> {model | game = g})

-- View

view : Model -> Html Msg
view =
  .game
  >> (ModelE.view GameView.view)
  >> Html.App.map GameMsg

main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
