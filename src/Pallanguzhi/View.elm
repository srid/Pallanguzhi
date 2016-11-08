module Pallanguzhi.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Board as Board

viewBoard : Board.Board -> Html Msg
viewBoard board =
  div []
    [ h2 [] [text "Player A"]
    , viewCups <| Board.cupsA board
    , h2 [] [text "Player B"]
    , viewCups <| List.reverse <| Board.cupsB board
    ]

viewCups : List Board.Cup -> Html Msg
viewCups =
  div [] << List.map viewCup

viewCup : Board.Cup -> Html Msg
viewCup (Board.Cup count) =
  span [class "cup"] [text <| toString count]
