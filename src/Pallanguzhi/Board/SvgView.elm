module Pallanguzhi.Board.SvgView exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

view : Html a
view = 
  svg 
    [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" 
    ]
    [ circle [ cx "60", cy "60", r "50" ] []
    ]