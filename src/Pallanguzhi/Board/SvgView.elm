module Pallanguzhi.Board.SvgView exposing (..)

import Html
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes as S
import Svg.Events exposing (..)

import Util.Diagram as D

import List

view : (Int -> a) -> Html a
view f = 
  let 
    svgMeta = 
      [ S.version "1.1", S.x "0", S.y "0", S.viewBox "0 0 250 100"]
    drawing = 
      board f
  in 
    svg svgMeta (drawing |> D.draw 2 2)

board : (Int -> a) -> D.Diagram a
board f =
  let 
    row = 
      pit f 42
      |> List.repeat 7
      |> D.hfold 5
    store' n =
      store (D.width row) 42
      `D.stack`
      (D.text [] (toString n) |> D.move 3 11)
  in 
    D.vfold 5 [store' 12, row, row, store' 143]
    
pit : (Int -> a) -> Int -> D.Diagram a
pit f c =
  let 
    radius = 
      12
    color = 
      "#60B5CC"
    seed dx dy = 
      D.circle [] 2 |> D.move (5 + dx) (7 + dy)
    seeds =
      -- XXX: one pit can have *more than* 6 seeds
      -- Perhaps make this generic for N number of seeds, with N displayed in a corner.
      let 
        row = (D.hfold 1 <| List.repeat 3 <| seed 0 0)
      in
        D.vfold 2 [row, row]
  in
    D.circle [S.fill color, onClick (f 3)] radius
    `D.stack`
    (D.text [] (toString c) |> D.move 3 16)  -- FIXME: make fixed size

store : Int -> Int -> D.Diagram a
store w seeds =
  let 
    h = 12
  in 
    D.rect [S.fill "#44ee55"] w h 
