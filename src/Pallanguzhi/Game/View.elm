module Pallanguzhi.Game.View exposing (view)

import Maybe
import Html exposing (..)

import Util.Diagram as D
import Svg exposing (svg)
import Svg.Attributes as S
import Svg.Events exposing (onClick)

import Pallanguzhi.Game.Model as Model
import Pallanguzhi.Game.Hand exposing (Hand)
import Pallanguzhi.Board.Model as Board

type alias PitClickF a
  =  Board.Player
  -> Board.PitLocation
  -> a

view : Model.Model -> Maybe String -> Html Model.Msg
view model error =
  let
    boardHtml = Model.getBoard model |> viewBoard
    stateHtml = viewState model
    errorHtml = viewError error
  in
    div [] [ boardHtml, stateHtml, errorHtml ]

viewBoard : Board.Model -> Html Model.Msg
viewBoard board =
  let
    rowUIOf player =
      board 
      -- xxx use elm-state to pass "current pit" info
      |> Board.mapRowOf player (viewPit Model.Play player)
      |> Board.displayOrder player
      |> D.hfold 5
    rowA =
      rowUIOf Board.A  
    rowB =
      rowUIOf Board.B  
    storeFor =
      viewStore (D.width rowA)
    boardUI =
      [ storeFor Board.B board.storeB 
      , rowB
      , rowA
      , storeFor Board.A board.storeA
      ]
      |> D.vfold 5
  in 
    viewDiagram boardUI

viewDiagram : D.Diagram a -> Html a
viewDiagram diagram =
  let 
    svgMeta = 
      [ S.version "1.1", S.x "0", S.y "0", S.viewBox "0 0 300 100"]
  in 
    svg svgMeta (diagram |> D.draw 2 2)
    
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

viewPit : PitClickF a 
       -> Board.Player 
       -> Board.PitLocation 
       -> Board.Pit 
       -> D.Diagram a
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

viewStore : Int -> Board.Player -> Int -> D.Diagram a
viewStore w player seeds =
  let 
    h = 12
    g = D.rect [S.fill "#44ee55"] w h 
    t = D.text [S.fontSize "12"] (toString player ++ ":" ++ toString seeds) 
        |> D.move 90 10
  in
    D.stack g t 