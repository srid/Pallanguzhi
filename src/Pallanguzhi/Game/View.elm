module Pallanguzhi.Game.View exposing (view)

import Html exposing (..)

import Pallanguzhi.Game.Model as Model
import Pallanguzhi.Board.View as BoardView

view : Model.Model -> Maybe String -> Html Model.Msg
view model errorMaybe =
  let
    boardHtml = BoardView.viewC Model.Play model.board 
    errorHtml = viewError errorMaybe
  in
  div [] [boardHtml, errorHtml]

viewError : Maybe String -> Html a
viewError errorMaybe =
  case errorMaybe of
    Nothing -> 
      div [] []
    Just e -> 
      div [] [ text <| "Error: " ++ e ] 