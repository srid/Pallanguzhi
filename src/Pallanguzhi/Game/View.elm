module Pallanguzhi.Game.View exposing (view)

import Maybe
import Html exposing (..)

import Pallanguzhi.Game.Model as Model
import Pallanguzhi.Board.View as BoardView

mapMaybe : (a -> Html b) -> Maybe a -> Html b
mapMaybe f =
  Maybe.map f >> Maybe.withDefault (text "Nothing")

view : Model.Model -> Maybe String -> Html Model.Msg
view model errorMaybe =
  let
    boardHtml = 
      BoardView.viewC Model.Play model.board 
    handHtml = 
      mapMaybe viewHand model.hand
    errorHtml = 
      viewError errorMaybe
  in
  div [] [boardHtml, handHtml, errorHtml]

viewHand : Model.Hand -> Html Model.Msg
viewHand hand =
  div [] 
    [ hr [] [] 
    , b [] [ text <| "Player: " ++ toString hand.player ]
    , span [] [ text <| ", Seeds: " ++ toString hand.seeds]
    , span [] [ text <| ", Pit: " ++ toString hand.loc]
    ]

viewError : Maybe String -> Html a
viewError errorMaybe =
  case errorMaybe of
    Nothing -> 
      div [] []
    Just e -> 
      div [] [ text <| "Error: " ++ e ] 