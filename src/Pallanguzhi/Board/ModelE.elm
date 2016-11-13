module Pallanguzhi.Board.ModelE exposing (..)

import Html exposing (Html)
import Return

import Util.ModelE as ModelE
import Util.ModelE exposing (ModelE)

import Pallanguzhi.Board.Model as Board
import Pallanguzhi.Board.View as View

type alias Model = ModelE String Board.Model

type alias Msg = Board.Msg

init : Model
init = ModelE.init Board.init

update
    : Board.Msg
    -> ModelE String Board.Model
    -> Return.Return Board.Msg (ModelE String Board.Model)
update = ModelE.update Board.updateR

view : ModelE String Board.Model -> Html Board.Msg
view = ModelE.view View.viewBoard
