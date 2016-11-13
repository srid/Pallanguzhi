module Pallanguzhi.BoardE exposing (..)

import Html exposing (Html)
import Return

import Util.ModelE as ModelE
import Util.ModelE exposing (ModelE)

import Pallanguzhi.Board as Board
import Pallanguzhi.View as View
import Pallanguzhi.Msg exposing (Msg)

type alias Model = ModelE String Board.Model

init : Model
init = ModelE.init Board.init

update
    : Board.Msg
    -> ModelE String Board.Model
    -> Return.Return Board.Msg (ModelE String Board.Model)
update = ModelE.update Board.updateR

view : ModelE String Board.Model -> Html Msg
view = ModelE.view View.viewBoard