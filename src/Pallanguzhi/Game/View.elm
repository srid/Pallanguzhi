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
    stateHtml = 
      viewState model.state
    errorHtml = 
      viewError errorMaybe
  in
  div [] [boardHtml, stateHtml, errorHtml]

viewState : Model.State -> Html Model.Msg 
viewState state =
  case state of
    Model.Awaiting player ->
      div [] [ text <| "Awaiting turn by player: " ++ toString player ]
    Model.Seeding hand ->
      viewHand hand
    Model.EndGame ->
      div [] [ text <| "Game ended" ]

viewHand : Model.Hand -> Html Model.Msg
viewHand hand =
  div [] 
    [ b [] [ text <| "Player: " ++ toString hand.player ]
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