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
      mapRows (viewPit f) board
      |> mapTuple (D.hfold 5)
    makeStore =
      viewStore (D.width rowA) 
  in 
    [makeStore Model.B board.storeB, rowB, rowA, makeStore Model.A board.storeA]
    |> D.vfold 5 

viewPit : PitClickF a -> Model.Player -> Model.PitLocation -> Model.Pit -> D.Diagram a
viewPit f player loc pit =
  -- XXX: clean up this ugliness.
  let 
    radius = 
      12
    color = 
      "#60B5CC"
    handleClick =
      f player loc
      |> onClick
    circle = 
      D.circle [S.fill color, handleClick] radius
    text =
      D.text [S.fontSize "12", handleClick] (toString pit.seeds) 
      |> D.move 5 16 
  in
    if pit.player == player then
      D.stack circle text
    else
      Debug.crash "incorrect player"

viewStore : Int -> Model.Player -> Int -> D.Diagram a
viewStore w player seeds =
  let 
    h = 12
    g = D.rect [S.fill "#44ee55"] w h 
    t = D.text [S.fontSize "12"] (toString player ++ ":" ++ toString seeds) 
        |> D.move 90 10
  in
    D.stack g t 

mapRows : (Model.Player -> Model.PitLocation -> Model.Pit -> b) -> Model -> (List b, List b)
mapRows f board =
  let 
    (a, b) = Model.rows board
  in
    -- Player B's rows need to be reverted matching left-to-right display.
    ( List.indexedMap (f Model.A) a
    , List.indexedMap (f Model.B) b |> List.reverse 
    )

mapTuple : (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) =
  (f x, f y)