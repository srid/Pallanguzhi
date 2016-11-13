import Html exposing (..)
import Html.App
import Return
import Return exposing (Return)

import Pallanguzhi.Board.ModelE as BoardComponent


-- Model

type alias Model =
  { board : BoardComponent.Model
  }

init : Return Msg Model
init = { board = BoardComponent.init }
       |> Return.singleton

-- Msg
      
type Msg 
  = NoOp
  | BoardMsg BoardComponent.Msg

-- Update

update : Msg -> Model -> Return Msg Model
update msg model =
  case msg of
    NoOp ->
      Return.singleton model
    BoardMsg action ->
      model
      |> .board
      |> BoardComponent.update action
      |> Return.mapBoth BoardMsg (\b -> {model | board = b})

-- View

view : Model -> Html Msg
view =
  .board
  >> BoardComponent.view 
  >> Html.App.map BoardMsg

main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
