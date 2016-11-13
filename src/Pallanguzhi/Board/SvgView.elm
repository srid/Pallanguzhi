module Pallanguzhi.Board.SvgView exposing (..)

import Html
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import List

view : (Int -> a) -> Html a
view f = 
  svg 
    [ version "1.1", x "0", y "0", viewBox "0 0 250 100" 
    ]
    <| 
    List.map (viewPit 20 f) [0..5]
    `List.append`
    List.map (viewPit 50 f) [0..5]

viewPit : Int -> (Int -> a) -> Int -> Html a
viewPit y f loc =
  let 
    start = 20
    radius = 12
    color  = "#60B5CC"
    x = start + loc * (radius * 2 + 5)
  in
    circle [ cx <| toString x 
           , cy <| toString y
           , r  <| toString radius
           , fill color
           , onClick (f 3) ] []
