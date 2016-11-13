module Pallanguzhi.Board.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Css
import Css exposing (hex)

import Pallanguzhi.Msg as Msg
import Pallanguzhi.Board.Model exposing (Model)
import Pallanguzhi.Board.Model as Model

viewBoard : Model -> Maybe String -> Html Msg.Msg
viewBoard board errorMaybe =
  let
    (pitsA, pitsB) = Model.rows board
  in 
    div []
      [ viewStore board.storeA
      , hr [] []
      , viewPits Model.A pitsA
      , hr [] []
      , viewPits Model.B (List.reverse pitsA)
      , hr [] []
      , viewStore board.storeB
      , hr [] []
      , viewError errorMaybe
      ]

viewError : Maybe String -> Html a
viewError errorMaybe =
  case errorMaybe of
    Nothing -> 
      div [] []
    Just e -> 
      div [] [ text <| "Error: " ++ e ]

viewPits : Model.Player -> List Model.Pit -> Html Msg.Msg
viewPits player =
  div [] << List.indexedMap (viewPit player)

viewPit : Model.Player -> Int -> Model.Pit -> Html Msg.Msg
viewPit player pitLoc pit =
  let
    s = styles
          [ Css.backgroundColor <| hex "11ff00"
          , Css.padding <| Css.em 1
          , Css.margin <| Css.px 1 ]
  in
    span [s, onClick <| Msg.Board <| Model.Play player pitLoc] [text <| toString pit.seeds]

viewStore : Int -> Html Msg.Msg
viewStore seeds = 
  let s = styles 
            [ Css.backgroundColor <| hex "aa3300"
            , Css.padding <| Css.em 1 ]
  in
    div [s] [text <| toString seeds]

styles : List Css.Mixin -> Attribute a
styles =
    Css.asPairs >> Html.Attributes.style
