module Dayakattai.View exposing (..)

import Html exposing (..)

import Dayakattai.Msg exposing (Msg)
import Dayakattai.Board as Board

viewBoard : Board.Model -> Html Msg
viewBoard board =
  div []
    [ h2 [] [text "Player A"]
    , viewPlayer board.playerA
    , h2 [] [text "Player B"]
    , viewPlayer board.playerB
    ]

viewPlayer : Board.Player -> Html Msg
viewPlayer =
  div [] << List.map viewChip << .chips

viewChip : Board.Chip -> Html Msg
viewChip chip =
  div [] [text <| toString chip.location]
