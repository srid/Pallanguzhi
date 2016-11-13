module Pallanguzhi.Board.SvgView exposing (..)

import Html
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Util.Diagram as D

import List

view : (Int -> a) -> Html a
view f = 
  let 
    row = 
      List.repeat 6 (pit f) |> D.hfold 5
    diagram =
      D.vfold 5 [row, row]
      |> D.draw 10 10 
  in 
    svg 
     [ version "1.1", x "0", y "0", viewBox "0 0 250 100" 
     ] diagram
    
pit : (Int -> a) -> D.Diagram a
pit f =
  let 
    radius = 12
    color  = "#60B5CC"
  in
    D.circle [fill color, onClick (f 3)] radius

viewStore : Int -> Int -> Html a
viewStore y' seeds =
  let 
    h = 5
    w = 100
    startX = 10
  in
    rect [ x <| toString startX
         , y <| toString <| 20 + y'
         , width <| toString w
         , height <| toString h
         ] []
