module Pallanguzhi.View exposing (view)

import Maybe
import Html exposing (..)

import Util.Diagram as D
import Svg exposing (svg)
import Svg.Attributes as S
import Svg.Events exposing (onClick)

import Pallanguzhi.Game as Game
import Pallanguzhi.Hand exposing (Hand)
import Pallanguzhi.Board as Board

type alias PitClickF a
  =  Board.Player
  -> Board.PitLocation
  -> a

-- FIXME: replace PitLocation (optional +7) confusion with Board.Cursor
type alias Config = 
  { focusPit    : Maybe Board.PitLocation
  , focusPlayer : Maybe Board.Player
  }

defaultConfig : Config
defaultConfig = { focusPit = Nothing, focusPlayer = Nothing }

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
  in
    div [] [ boardHtml, stateHtml, errorHtml ]

viewBoard : Game.Model -> Html Game.Msg
viewBoard model =
  let
    board = 
      Game.getBoard model
    rowUIOf player =
      board 
      -- xxx use elm-state to pass "current pit" info
      |> Board.mapRowOf player (viewPit Game.Play model player)
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
    
viewState : Game.Model -> Html Game.Msg 
viewState state =
  case state of
    Game.Awaiting player _ ->
      div [] [ text <| "Awaiting turn by player: " ++ toString player ]
    Game.Seeding hand _ ->
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

viewPit : PitClickF a 
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
    focusPit = 
      case config.focusPit of
        Just focusLoc -> focusLoc == Board.locFor player loc
        Nothing -> False
    radius = 
      12
    color = 
      if focusPit then "#B5CC60" else "#60B5CC"
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