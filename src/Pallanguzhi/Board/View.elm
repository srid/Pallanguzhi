module Pallanguzhi.Board.View exposing (..)

import Svg.Attributes as S
import Svg.Events exposing (..)
import Debug

import Util.Diagram as D

import Pallanguzhi.Board.Model exposing (Model)
import Pallanguzhi.Board.Model as Model

type alias PitClickF a
  =  Model.Player
  -> Model.PitLocation
  -> a

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