module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type alias Board =
  { pits   : Array Pit
  , storeA : Int
  , storeB : Int
  }

type Player = A | B 
type alias Pit = { player : Player, seeds : Int}

pitsPerPlayer : number
pitsPerPlayer = 7

seedsPerPit : number
seedsPerPit = 12

initialBoard : Board
initialBoard = 
  let 
    makeRow player = Array.repeat pitsPerPlayer {player = player, seeds = seedsPerPit}
  in
    { pits = makeRow A `Array.append` makeRow B
    , storeA = 0
    , storeB = 0
    }
    |> dig 0

rows : Board -> (List Pit, List Pit)
rows board = 
  let 
    f g = Array.toList >> g pitsPerPlayer
  in 
    ( f List.take board.pits
    , f List.drop board.pits
    )

lookup : Int -> Board -> Pit
lookup idx board = 
  case Array.get idx board.pits of
    Just pit -> 
      pit
    Nothing  -> 
      -- Invalid index is only possible due to programmer error.
      Debug.crash <| "error: invalid index: " ++ (toString idx)

next : Int -> Int
next idx
  = let 
      total = 2 * pitsPerPlayer 
    in 
      (idx + 1) % total

updateSeeds : Int -> (Int -> Int) -> Board -> Board
updateSeeds idx f board =
  let 
    pit = lookup idx board
    pits = Array.set idx { pit | seeds = f pit.seeds } board.pits
  in
    { board | pits = pits }

inc : Int -> Board -> Board
inc idx board =
  updateSeeds idx (\s -> s + 1)  board

clear : Int -> Board -> Board
clear idx board =
  updateSeeds idx (always 0) board

store : Player -> Int -> Board -> Board
store player seeds board =
  case player of
    A -> { board | storeA = board.storeA + seeds }
    B -> { board | storeA = board.storeB + seeds }

capture : Player -> Int -> Board -> Board
capture player idx board = 
  let 
    c = lookup idx board |> .seeds
  in
    board |> clear idx |> store player c 

dig : Int -> Board -> Board
dig idx board =
  let 
    pit = lookup idx board
  in
    digAs pit.player idx board

digAs : Player -> Int -> Board -> Board
digAs player idx board =
  let
    hand = lookup idx board |> .seeds
  in
    board 
    |> clear    idx 
    |> sowAs player hand (next idx)

sowAs : Player -> Int -> Int -> Board -> Board
sowAs player hand idx board =
  let 
    lk idx = board |> lookup idx |> .seeds
    idxN   = next idx
    idxNN  = next idxN
  in
    board
    |> case (hand, lk idx, lk idxN) of
      (0, 0, 0) -> -- No hand, next two pits empty. End turn.
        identity
      (0, 0, _) -> -- Empty pit. Capture next and move on.
        capture player idxN
        >> digAs player idxNN
      (0, _, _) -> -- Continue digging.
        digAs player idx
      _ -> -- Sow 1 seed and continue digging.
        inc idx
        >> sowAs player (hand-1) idx