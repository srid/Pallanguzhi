module Pallanguzhi.Game.View exposing (view)

import Maybe
import Html exposing (..)

import Pallanguzhi.Game.Model as Model
import Pallanguzhi.Game.Hand exposing (Hand)
import Pallanguzhi.Board.View as BoardView

mapMaybe : (a -> Html b) -> Maybe a -> Html b
mapMaybe f =
  Maybe.map f >> Maybe.withDefault (text "Nothing")

view : Model.Model -> Maybe String -> Html Model.Msg
view model errorMaybe =
  let
    boardHtml = 
      BoardView.viewC Model.Play (Model.getBoard model)
    stateHtml = 
      viewState model
    errorHtml = 
      viewError errorMaybe
  in
  div [] [boardHtml, stateHtml, errorHtml]

viewState : Model.Model -> Html Model.Msg 
viewState state =
  case state of
    Model.Awaiting player _ ->
      div [] [ text <| "Awaiting turn by player: " ++ toString player ]
    Model.Seeding hand _ ->
      viewHand hand
    Model.EndGame _ ->
      div [] [ text <| "Game ended" ]

viewHand : Hand -> Html Model.Msg
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