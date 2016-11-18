module Pallanguzhi.Board2 exposing (..)

import Array exposing (Array)
import Array
import List.Extra

import Monocle.Lens exposing (Lens)
import Monocle.Lens as Lens
import Monocle.Optional exposing (Optional)
import Monocle.Optional as Optional
import Monocle.Common as MC

type alias Pit = Int
type alias Store = Int

type alias Player = 
  { pits : Array Pit 
  , store : Int 
  }

type alias Board = 
  { a : Player 
  , b : Player 
  }

init : Board
init = { a = { pits = [9,1,0,8,2,0,12] |> Array.fromList, store = 23},
         b = { pits = [3,6,3,4,0,3,11] |> Array.fromList, store = 42} }

type alias Cursor = Lens Board Pit

aLens : Lens Board Player
aLens = Lens .a (\v r -> { r | a = v})

bLens : Lens Board Player
bLens = Lens .b (\v r -> { r | b = v})

pitsLens : Lens Player (Array Int)
pitsLens = Lens .pits (\v r -> { r | pits = v})

aPitsLens : Lens Board (Array Pit)
aPitsLens = Lens.compose aLens pitsLens

bPitsLens : Lens Board (Array Pit)
bPitsLens = Lens.compose bLens pitsLens

aPit : Int -> Optional Board Pit
aPit = Optional.compose (Optional.fromLens aPitsLens) << MC.array

bPit : Int -> Optional Board Pit
bPit = Optional.compose (Optional.fromLens bPitsLens) << MC.array

optionalToLens : Optional a b -> Lens a b
optionalToLens optional = 
  Lens (optional.getOption >> crashOnNothing) optional.set

crashOnNothing : Maybe a -> a
crashOnNothing m = 
  case m of 
    Nothing -> Debug.crash "Nothing"
    Just v  -> v

-- Global list that does not change. Elements, which are merely 
-- pointers (lens), can be passed around
pitCursors : List Cursor
pitCursors = 
  let indices = List.range 0 6 
      aPits   = List.map (aPit >> optionalToLens) indices
      bPits   = List.map (bPit >> optionalToLens) indices
  in  aPits ++ bPits

cursor1 : Cursor
cursor1 = pitCursors |> List.head |> crashOnNothing

next : Cursor -> Maybe Cursor
next cursor =
  circularListNext cursor pitCursors

circularListNext : a -> List a -> Maybe a 
circularListNext e list =
  List.head list
  |> Maybe.andThen (\first ->
    let f xs =
      case xs of
        [] -> Nothing
        first :: rest -> if e == first then (List.head rest) else f rest
    in  
      f <| list ++ [first])

nextInCircularList : Cursor -> List Cursor -> Cursor
nextInCircularList e list = 
  let n   = List.length list
      idx = List.Extra.findIndex ((==) e) list
      idy = Maybe.map (\idx -> (idx + 1) % n) idx
  in  Maybe.andThen ((flip List.Extra.getAt) list) idy
      |> crashOnNothing