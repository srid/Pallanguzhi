module Pallanguzhi.Board.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Css
import Css exposing (hex)

import Pallanguzhi.Board.Model exposing (Model)
import Pallanguzhi.Board.Model as Model

type alias PitClickF msg
  =  Model.Player 
  -> Model.PitLocation 
  -> msg 

viewC : PitClickF a -> Model -> Html a
viewC f board =
  let
    (pitsA, pitsB) = Model.rows board
  in 
    div []
      [ viewStore board.storeA
      , hr [] []
      , viewPits f Model.A pitsA
      , hr [] []
      , viewPits f Model.B (List.reverse pitsA)
      , hr [] []
      , viewStore board.storeB
      ]

viewPits : PitClickF a -> Model.Player -> List Model.Pit -> Html a
viewPits f player =
  div [] << List.indexedMap (viewPit f player)

viewPit : PitClickF a -> Model.Player -> Int -> Model.Pit -> Html a
viewPit f player pitLoc pit =
  let
    s = styles
          [ Css.backgroundColor <| hex "11ff00"
          , Css.padding <| Css.em 1
          , Css.margin <| Css.px 1 ]
  in
    span 
      [s, onClick <| f player pitLoc] 
      [text <| toString pit.seeds]

viewStore : Int -> Html a
viewStore seeds = 
  let s = styles 
            [ Css.backgroundColor <| hex "aa3300"
            , Css.padding <| Css.em 1 ]
  in
    div [s] [text <| toString seeds]

styles : List Css.Mixin -> Attribute a
styles =
    Css.asPairs >> Html.Attributes.style
