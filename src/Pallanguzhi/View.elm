module Pallanguzhi.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Css
import Css exposing (hex)

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Board as Board

viewBoard : Board.Board -> Html Msg
viewBoard board =
  let
    (pitsA, pitsB) = Board.rows board
  in 
    div []
      [ h2 [] [text "Player A"]
      , viewPits pitsA
      , h2 [] [text "Player B"]
      , viewPits (List.reverse pitsA)
      ]

viewPits : List Board.Pit -> Html Msg
viewPits =
  div [] << List.map viewPit

viewPit : Board.Pit -> Html Msg
viewPit pit =
  let
    s = styles
          [ Css.backgroundColor <| hex "11ff00"
          , Css.padding <| Css.em 1 ]
  in
    span [s] [text <| toString pit.seeds]

styles : List Css.Mixin -> Attribute a
styles =
    Css.asPairs >> Html.Attributes.style
