module Pallanguzhi.View exposing (view)

import Maybe
import Html exposing (..)
import Html.Attributes as A

import Util.Diagram as D
import Svg exposing (svg)
import Svg.Attributes as S
import Svg.Events exposing (onClick)

import Pallanguzhi.Game as Game
import Pallanguzhi.Hand exposing (Hand)
import Pallanguzhi.Hand as Hand
import Pallanguzhi.Board as Board

type alias Config = 
  { focusPit    : Maybe Board.PitLocation
  , focusPlayer : Maybe Board.Player
  }

defaultConfig : Config
defaultConfig = { focusPit = Nothing, focusPlayer = Nothing }

focussingPit : Config -> Board.PitLocation -> Bool
focussingPit config loc = config.focusPit == Just loc

focussingPlayer : Config -> Board.Player -> Bool
focussingPlayer config player =
  case config.focusPlayer of
    Just focusPlayer -> focusPlayer == player
    Nothing -> False

configFor : Game.Model -> Config 
configFor model =
  case model of 
    Game.Awaiting player board ->
      { defaultConfig | focusPlayer = Just player }
    Game.Seeding hand board ->
      { defaultConfig | focusPlayer = Just hand.player
                      , focusPit = Just hand.loc }
    Game.EndGame board ->
      defaultConfig

view : Game.Model -> Maybe String -> Html Game.Msg
view model error =
  let
    boardHtml = viewBoard model
    stateHtml = viewState model
    errorHtml = viewError error
    soundHtml = viewSound model
  in
    div [] [ boardHtml, stateHtml, errorHtml, soundHtml ]

viewBoard : Game.Model -> Html Game.Msg
viewBoard model =
  let
    board = 
      Game.getBoard model
    rowUIOf player =
      board 
      |> Board.mapRowOf player (viewPit Game.Play model player)
      |> Board.displayOrder player
      |> D.hfold 5
    rowA =
      rowUIOf Board.A  
    rowB =
      rowUIOf Board.B  
    storeFor =
      viewStore model (D.width rowA)
    boardUI =
      [ storeFor Board.B 
      , rowB
      , rowA
      , storeFor Board.A
      ]
      |> D.vfold 5
  in 
    viewDiagram boardUI

viewSound : Game.Model -> Html Game.Msg
viewSound model =
  case model of 
    Game.Seeding hand _ ->
      if Hand.didCapture hand then
        div [A.id "x" ]
          [ audio [A.src "/data/coin.wav", A.loop False, A.autoplay True ] []
          ]
      else
        div [] []
    _ ->
      div [] []

viewDiagram : D.Diagram a -> Html a
viewDiagram diagram =
  let 
    svgMeta = 
      [ S.version "1.1", S.x "0", S.y "0", S.viewBox "0 0 300 100"]
  in 
    svg svgMeta (diagram |> D.draw 2 2)
    
viewState : Game.Model -> Html Game.Msg 
viewState state =
  case state of
    Game.Awaiting player _ ->
      div [] [ text <| "Awaiting turn by player: " ++ toString player ]
    Game.Seeding hand _ ->
      -- TODO: emphasized UI for hand seed count
      viewHand hand
    Game.EndGame _ ->
      div [] [ text <| "Game ended" ]

viewHand : Hand -> Html Game.Msg
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

viewPit : (Board.PitLocation -> a)
       -> Game.Model
       -> Board.Player 
       -> Board.PitLocation 
       -> Board.Pit 
       -> D.Diagram a
viewPit f model player loc pit =
  -- XXX: clean up this ugliness.
  let 
    config =
      configFor model
    radius = 
      12
    color = 
      if focussingPit config loc then "orange" else "#60B5CC"
    handleClick =
      f loc
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

viewStore : Game.Model -> Int -> Board.Player -> D.Diagram a
viewStore model w player =
  let 
    config = configFor model
    board = Game.getBoard model
    seeds = Board.storeFor player board
    label = toString player ++ ":" ++ toString seeds
    color = if focussingPlayer config player then "orange" else "grey" 
    h = 12
    g = D.rect [S.fill color] w h 
    t = D.text [S.fontSize "12"] label
        |> D.move 90 10
  in
    D.stack g t 