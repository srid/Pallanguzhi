module Pallanguzhi.Hand exposing (..)

import Pallanguzhi.Board exposing (Board)
import Pallanguzhi.Board as Board

type alias Hand = 
  { player : Board.Player 
  , seeds : Int
  , loc : Board.PitLocation
  }

new : Board.Player -> Board.PitLocation -> Board -> Result String Hand
new player loc board =
  let 
    pit = 
      Board.lookup loc board
    hand =
      { player = pit.player
      , seeds = 0
      , loc = loc 
      }
  in 
    if player /= pit.player then
      Err "Wrong player"
    else if pit.seeds == 0 then
      Err "Nothing to lift from empty pit"
    else
      Ok hand

move : Hand -> Board -> (Maybe Hand, Board)
move hand board =
  let 
    seedsBelow =
      Board.lookupSeeds hand.loc board
    seedsNext =
      Board.lookupSeeds (Board.next hand.loc) board
    seedsNextNext = 
      Board.lookupSeeds (Board.next <| Board.next hand.loc) board
    keepSeeding (hand, board) =
      (Just hand, board)
    awaitForOpponent (hand, board) =
      (Nothing, board)
  in
    case (hand.seeds, seedsBelow, seedsNext, seedsNextNext) of
      (0, 0, 0, _) -> -- No hand, next two pits empty. End turn.
        -- TODO: determine EndGame
        (hand, board)
        |> awaitForOpponent
      (0, 0, s, 0) -> -- Capture and end turn.
        (hand, board)
        |> advance 
        |> capture
        |> awaitForOpponent
      (0, 0, s, _) -> -- Capture and continue.
        (hand, board)
        |> advance 
        |> capture
        |> advance 
        |> keepSeeding
      (0, s, _, _) -> -- Continue digging.
        (hand, board)
        |> lift
        |> advance
        |> keepSeeding
      (s, 3, _, _) -> -- Pasu; capture!
        (hand, board)
        |> sow  -- Sow 1 before capturing the 6 seeds
        |> capture
        |> advance
        |> keepSeeding
      (s, _, _, _) -> -- Sow 1 seed and continue digging.
        (hand, board)
        |> sow
        |> advance
        |> keepSeeding

capture : (Hand, Board) -> (Hand, Board)
capture (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in
    ( hand
    , board
      |> Board.clear hand.loc
      |> Board.store hand.player seeds
    )

lift : (Hand, Board) -> (Hand, Board)
lift (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in 
    ( { hand | seeds = seeds }
    , Board.clear hand.loc board
    )

sow : (Hand, Board) -> (Hand, Board)
sow (hand, board) =
  ( { hand | seeds = hand.seeds - 1 }
  , Board.inc hand.loc board
  )

advance : (Hand, Board) -> (Hand, Board)
advance (hand, board) =
  let 
    newHand = { hand | loc = Board.next hand.loc }
  in
    (newHand, board)
