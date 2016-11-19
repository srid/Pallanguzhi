module Pallanguzhi.Hand exposing (..)

import Pallanguzhi.Board exposing (Board)
import Pallanguzhi.Board as Board

type alias Hand = 
  { player : Board.Player 
  , seeds : Int
  , loc : Board.PitLocation
  , lastState : Maybe State
  }

type State
  = EndTurn
  | CaptureAndEndTurn
  | CaptureAndContinue
  | Lift
  | CapturePasu
  | SowAndContinue

type alias TransformF = (Hand, Board) -> (Hand, Board)


new : Board.Player -> Board.PitLocation -> Board -> Result String Hand
new player loc board =
  let 
    pit = 
      Board.lookup loc board
    hand =
      { player = pit.player
      , seeds = 0
      , loc = loc 
      , lastState = Nothing
      }
  in 
    if player /= pit.player then
      Err "Wrong player"
    else if pit.seeds == 0 then
      Err "Nothing to lift from empty pit"
    else
      Ok hand


state : Hand -> Board -> State
state hand board =
  let 
    seedsBelow =
      Board.lookupSeeds hand.loc board
    seedsNext =
      Board.lookupSeeds (Board.next hand.loc) board
    seedsNextNext = 
      Board.lookupSeeds (Board.next <| Board.next hand.loc) board
  in
    case (hand.seeds, seedsBelow, seedsNext, seedsNextNext) of
      (0, 0, 0, _) -> -- No hand, next two pits empty. End turn.
        EndTurn
      (0, 0, s, 0) -> -- Capture and end turn.
        CaptureAndEndTurn
      (0, 0, s, _) -> -- Capture and continue.
        CaptureAndContinue
      (0, s, _, _) -> -- Lift seeds
        Lift
      (s, 3, _, _) -> -- Pasu; capture!
        CapturePasu
      (s, _, _, _) -> -- Sow 1 seed and continue digging.
        SowAndContinue


move : TransformF
move (hand, board) =
  let 
    st = state hand board
    f = transitions (hand, board) st
  in 
    (hand, board)
    |> setState st
    |> f


transitions : (Hand, Board) -> State -> TransformF
transitions (hand, board) st =
  case st of 
    EndTurn -> 
      identity
    CaptureAndEndTurn -> 
      advance >> capture 
    Lift ->
      lift >> advance
    CaptureAndContinue -> 
      advance >> capture >> advance 
    CapturePasu ->
      -- XXX: shoud make this two step? So user can see the '4' 
      -- before it vanishes.
      sow >> capture >> advance
    SowAndContinue ->  
      sow >> advance


wasState : List State -> Hand -> Bool
wasState states hand =
  hand.lastState
  |> Maybe.map (\st -> List.any ((==) st) states)
  |> Maybe.withDefault False

shouldEndTurn : Hand -> Bool
shouldEndTurn = wasState [EndTurn, CaptureAndEndTurn]

didCapture : Hand -> Bool
didCapture = wasState [CaptureAndEndTurn, CaptureAndContinue, CapturePasu]

setState : State -> TransformF
setState st (hand, board) = 
  ({hand | lastState = Just st}, board)
  
capture : TransformF
capture (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in
    ( hand
    , board
      |> Board.clear hand.loc
      |> Board.store hand.player seeds
    )

lift : TransformF
lift (hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in 
    ( { hand | seeds = seeds }
    , Board.clear hand.loc board
    )

sow : TransformF
sow (hand, board) =
  ( { hand | seeds = hand.seeds - 1 }
  , Board.inc hand.loc board
  )

advance : TransformF
advance (hand, board) =
  let 
    newHand = { hand | loc = Board.next hand.loc }
  in
    (newHand, board)
