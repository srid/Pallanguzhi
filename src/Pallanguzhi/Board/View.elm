module Pallanguzhi.Board.View exposing (..)

import Html
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes as S
import Svg.Events exposing (..)
import List
import Debug

import Util.Diagram as D

import Pallanguzhi.Board.Model exposing (Model)
import Pallanguzhi.Board.Model as Model

type alias PitClickF a
  =  Model.Player 
  -> Model.PitLocation 
  -> a 

viewC : PitClickF a -> Model -> Html a
viewC f board = 
  let 
    svgMeta = 
      [ S.version "1.1", S.x "0", S.y "0", S.viewBox "0 0 300 100"]
    drawing = 
      viewBoard f board
  in 
    svg svgMeta (drawing |> D.draw 2 2)

viewBoard : PitClickF a -> Model ->  D.Diagram a
viewBoard f board =
  let 
    (rowA, rowB) =
      let 
        (a, b) = Model.rows board
      in
        (viewPits f Model.A a, viewPits f Model.B b)
    makeStore =
      viewStore (D.width rowA) 
  in 
    D.vfold 5 [makeStore board.storeB, rowB, rowA, makeStore board.storeA]

viewPits : PitClickF a -> Model.Player -> List Model.Pit -> D.Diagram a
viewPits f player =
  List.indexedMap (viewPit f player) 
  >> (case player of
      Model.A -> 
        identity
      Model.B ->  -- Reverse second player row (top) due to going counter clockwise
        List.reverse)
  >> D.hfold 5
        
    
viewPit : PitClickF a -> Model.Player -> Model.PitLocation -> Model.Pit -> D.Diagram a
viewPit f player loc pit =
  let 
    radius = 
      12
    color = 
      "#60B5CC"
    handleClick =
      onClick <| f player loc
    seed dx dy = 
      D.circle [] 2 |> D.move (5 + dx) (7 + dy)
    seeds =
      -- XXX: one pit can have *more than* 6 seeds
      -- Perhaps make this generic for N number of seeds, with N displayed in a corner.
      let 
        row = (D.hfold 1 <| List.repeat 3 <| seed 0 0)
      in
        D.vfold 2 [row, row]
    circle = 
      D.circle [S.fill color, handleClick] radius
    text =
      (D.text [S.fontSize "12", handleClick] (toString pit.seeds) |> D.move 5 16)  -- FIXME: make fixed size
  in
    if pit.player == player then
      D.stack circle text
    else
      Debug.crash "incorrect player"

viewStore : Int -> Int -> D.Diagram a
viewStore w seeds =
  let 
    h = 12
    g = D.rect [S.fill "#44ee55"] w h 
    t = D.text [S.fontSize "12"] (toString seeds) |> D.move 90 10
  in
    D.stack g t 
