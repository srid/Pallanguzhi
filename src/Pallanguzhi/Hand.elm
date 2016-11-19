module Pallanguzhi.Hand exposing (..)

import Pallanguzhi.Board exposing (Board)
import Pallanguzhi.Board as Board

type Hand = Hand HandR

type alias HandR =
  { player : Board.Player 
  , seeds : Int
  , loc : Board.PitLocation
  , lastState : Maybe State
  , transitionQueue : List TransformF
  }

type State
  = EndTurn
  | CaptureAndEndTurn
  | CaptureAndContinue
  | Lift
  | CapturePasu
  | SowAndContinue

type alias TransformF = (Hand, Board) -> (Hand, Board)

getHand : Hand -> HandR
getHand (Hand h) = h

new : Board.Player -> Board.PitLocation -> Board -> Result String Hand
new player loc board =
  let 
    pit = 
      Board.lookup loc board
    hand = Hand
      { player = pit.player
      , seeds = 0
      , loc = loc 
      , lastState = Nothing
      , transitionQueue = []
      }
  in 
    if player /= pit.player then
      Err "Wrong player"
    else if pit.seeds == 0 then
      Err "Nothing to lift from empty pit"
    else
      Ok hand


state : Hand -> Board -> State
state (Hand hand) board =
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


enqueueTransitions : TransformF
enqueueTransitions (hand, board) =
  let 
    st = 
      state hand board
    tl = 
      getTransitions (hand, board) st
    h = 
      getHand hand
    newHand =
      Hand { h | lastState = Just st, transitionQueue = tl }
  in 
    (newHand, board)

setTransitions : List TransformF -> TransformF 
setTransitions tl (Hand hand, board) =
  (Hand { hand | transitionQueue = tl }, board)

runHand : TransformF
runHand (hand, board) =
  (hand, board)
  |> case (getHand hand).transitionQueue of 
    f :: xs 
      -> setTransitions xs >> f
    _    
      -> enqueueTransitions >> runHand

getTransitions : (Hand, Board) -> State -> List TransformF
getTransitions (hand, board) st =
  case st of 
    EndTurn -> 
      [identity]
    CaptureAndEndTurn -> 
      [advance, capture]
    Lift ->
      [lift, advance]
    CaptureAndContinue -> 
      [advance, capture, advance]
    CapturePasu ->
      -- XXX: shoud make this two step? So user can see the '4' 
      -- before it vanishes.
      [sow, capture, advance]
    SowAndContinue ->  
      [sow, advance]


wasState : List State -> Hand -> Bool
wasState states hand =
  (getHand hand).lastState
  |> Maybe.map (\st -> List.any ((==) st) states)
  |> Maybe.withDefault False

shouldEndTurn : Hand -> Bool
shouldEndTurn hand = 
  (wasState [EndTurn, CaptureAndEndTurn] hand) 
  && List.isEmpty (getHand hand).transitionQueue

-- XXX: update
didCapture : Hand -> Bool
didCapture = wasState [CaptureAndEndTurn, CaptureAndContinue, CapturePasu]

setState : State -> TransformF
setState st (Hand hand, board) = 
  (Hand {hand | lastState = Just st}, board)
  
capture : TransformF
capture (Hand hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in
    ( Hand hand
    , board
      |> Board.clear hand.loc
      |> Board.store hand.player seeds
    )

lift : TransformF
lift (Hand hand, board) =
  let 
    seeds = Board.lookupSeeds hand.loc board
  in 
    ( Hand { hand | seeds = seeds }
    , Board.clear hand.loc board
    )

sow : TransformF
sow (Hand hand, board) =
  ( Hand { hand | seeds = hand.seeds - 1 }
  , Board.inc hand.loc board
  )

advance : TransformF
advance (Hand hand, board) =
  let 
    newHand = { hand | loc = Board.next hand.loc }
  in
    (Hand newHand, board)
