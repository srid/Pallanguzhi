module Pallanguzhi.Game.Hand exposing (..)

import Pallanguzhi.Board.Model as Board

type alias Hand = 
  { player : Board.Player 
  , seeds : Int
  , loc : Board.PitLocation
  }

new : Board.Player -> Board.PitLocation -> Board.Model -> Result String Hand
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

move : Hand -> Board.Model -> (Maybe Hand, Board.Model)
move hand board =
  let 
    seedsBelow =
      Board.lookupSeeds hand.loc board
    seedsNext =
      Board.lookupSeeds (Board.next hand.loc) board
    keepSeeding (hand, board) =
      (Just hand, board)
    awaitForOtherPlayer (hand, board) =
      (Nothing, board)
  in
    case (hand.seeds, seedsBelow, seedsNext) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        -- TODO: determine EndGame
        (hand, board)
        |> awaitForOtherPlayer
      (0, 0, s) -> -- Empty pit. Capture next and move on.
        (hand, board)
        |> advance 
        |> capture
        |> advance 
        |> keepSeeding
      (0, s, _) -> -- Continue digging.
        (hand, board)
        |> lift
        |> advance
        |> keepSeeding
      (s, 5, _) -> -- Pasu; capture!
        (hand, board)
        |> sow  -- Sow 1 before capturing the 6 seeds
        |> capture
        |> advance
        |> keepSeeding
      (s, _, _) -> -- Sow 1 seed and continue digging.
        (hand, board)
        |> sow
        |> advance
        |> keepSeeding


capture : (Hand, Board.Model) -> (Hand, Board.Model)
capture (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in
    ( hand
    , board
      |> Board.clear hand.loc
      |> Board.store hand.player seeds
    )

lift : (Hand, Board.Model) -> (Hand, Board.Model)
lift (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in 
    ( { hand | seeds = seeds }
    , Board.clear hand.loc board
    )

sow : (Hand, Board.Model) -> (Hand, Board.Model)
sow (hand, board) =
  ( { hand | seeds = hand.seeds - 1 }
  , Board.inc hand.loc board
  )

advance : (Hand, Board.Model) -> (Hand, Board.Model)
advance (hand, board) =
  let 
    newHand = { hand | loc = Board.next hand.loc }
  in
    (newHand, board)
