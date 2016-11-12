module Pallanguzhi.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Css
import Css exposing (hex)

import Pallanguzhi.Msg exposing (Msg)
import Pallanguzhi.Board as Board

viewBoard : Board.Model -> Html Msg
viewBoard board =
  let
    (pitsA, pitsB) = Board.rows board
  in 
    div []
      [ viewStore board.storeA
      , hr [] []
      , viewPits pitsA
      , hr [] []
      , viewPits (List.reverse pitsA)
      , hr [] []
      , viewStore board.storeB
      ]

viewPits : List Board.Pit -> Html Msg
viewPits =
  div [] << List.map viewPit

viewPit : Board.Pit -> Html Msg
viewPit pit =
  let
    s = styles
          [ Css.backgroundColor <| hex "11ff00"
          , Css.padding <| Css.em 1
          , Css.margin <| Css.px 1 ]
  in
    span [s] [text <| toString pit.seeds]

viewStore : Int -> Html Msg
viewStore seeds = 
  let s = styles 
            [ Css.backgroundColor <| hex "aa3300"
            , Css.padding <| Css.em 1 ]
  in
    div [s] [text <| toString seeds]

styles : List Css.Mixin -> Attribute a
styles =
    Css.asPairs >> Html.Attributes.style
